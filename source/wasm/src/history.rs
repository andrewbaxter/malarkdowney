//! Undo/redo history.
//!
//! The editor re-renders lines itself, which breaks the browser's built in undo,
//! so it keeps its own history: a list of snapshots of the editor lines. A step is
//! recorded after each batch of changes (i.e. after a pause in typing, or after a
//! paste/cut).
use std::rc::Rc;

/// Maximum number of undo steps kept
const MAX_STEPS: usize = 200;

/// A selection as (line index, UTF-16 offset in line) pairs
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct SelPos {
    pub anchor: (usize, usize),
    pub focus: (usize, usize),
}

struct Step {
    /// Unchanged lines are shared with neighboring steps
    lines: Vec<Rc<str>>,
    /// Selection before the change that produced this step started (where to put the
    /// cursor when undoing it)
    sel_before: Option<SelPos>,
    /// Selection after the change (where to put the cursor when redoing it)
    sel_after: Option<SelPos>,
}

/// What to restore the editor to after undo/redo
pub struct Restore {
    pub lines: Vec<String>,
    pub sel: Option<SelPos>,
}

pub struct History {
    steps: Vec<Step>,
    at: usize,
    pending_sel_before: Option<SelPos>,
}

/// Reuse identical lines from `prev` (matching from the start and from the end)
fn share(prev: &[Rc<str>], new: Vec<String>) -> Vec<Rc<str>> {
    let prefix = prev.iter().zip(&new).take_while(|(a, b)| ***a == **b).count();
    let max_suffix = prev.len().min(new.len()) - prefix;
    let suffix =
        prev.iter().rev().zip(new.iter().rev()).take(max_suffix).take_while(|(a, b)| ***a == **b).count();
    let new_len = new.len();
    return new.into_iter().enumerate().map(|(i, line)| {
        if i < prefix {
            return prev[i].clone();
        } else if i >= new_len - suffix {
            return prev[prev.len() - (new_len - i)].clone();
        } else {
            return Rc::from(line);
        }
    }).collect();
}

impl History {
    pub fn new(lines: Vec<String>, sel: Option<SelPos>) -> Self {
        return History {
            steps: vec![Step {
                lines: share(&[], lines),
                sel_before: None,
                sel_after: sel,
            }],
            at: 0,
            pending_sel_before: None,
        };
    }

    /// Forget all history, starting again from `lines`
    pub fn reset(&mut self, lines: Vec<String>, sel: Option<SelPos>) {
        *self = History::new(lines, sel);
    }

    /// Note the selection before an edit, if this is the first edit since the last
    /// recorded step
    pub fn begin(&mut self, sel: Option<SelPos>) {
        if self.pending_sel_before.is_none() {
            self.pending_sel_before = sel;
        }
    }

    /// Record the current state as a new step, if it's different from the current
    /// step. Returns whether a step was recorded. Discards anything that could be
    /// redone.
    pub fn record(&mut self, lines: Vec<String>, sel: Option<SelPos>) -> bool {
        let current = &self.steps[self.at];
        if current.lines.len() == lines.len() && current.lines.iter().zip(&lines).all(|(a, b)| **a == **b) {
            return false;
        }
        let lines = share(&current.lines, lines);
        let sel_before = self.pending_sel_before.take().or(current.sel_after);
        self.steps.truncate(self.at + 1);
        self.steps.push(Step {
            lines: lines,
            sel_before: sel_before,
            sel_after: sel,
        });
        self.at += 1;
        if self.steps.len() > MAX_STEPS {
            self.steps.remove(0);
            self.at -= 1;
        }
        return true;
    }

    pub fn can_undo(&self) -> bool {
        return self.at > 0;
    }

    pub fn can_redo(&self) -> bool {
        return self.at + 1 < self.steps.len();
    }

    pub fn undo(&mut self) -> Option<Restore> {
        if !self.can_undo() {
            return None;
        }
        self.pending_sel_before = None;
        let undone_sel_before = self.steps[self.at].sel_before;
        self.at -= 1;
        let step = &self.steps[self.at];
        return Some(Restore {
            lines: step.lines.iter().map(|l| l.to_string()).collect(),
            sel: undone_sel_before.or(step.sel_after),
        });
    }

    pub fn redo(&mut self) -> Option<Restore> {
        if !self.can_redo() {
            return None;
        }
        self.pending_sel_before = None;
        self.at += 1;
        let step = &self.steps[self.at];
        return Some(Restore {
            lines: step.lines.iter().map(|l| l.to_string()).collect(),
            sel: step.sel_after,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lines(s: &[&str]) -> Vec<String> {
        return s.iter().map(|x| x.to_string()).collect();
    }

    fn sel(line: usize, offset: usize) -> Option<SelPos> {
        return Some(SelPos {
            anchor: (line, offset),
            focus: (line, offset),
        });
    }

    #[test]
    fn undo_redo() {
        let mut h = History::new(lines(&["a"]), sel(0, 1));
        assert!(!h.can_undo());
        assert!(!h.record(lines(&["a"]), sel(0, 1)), "unchanged isn't a step");
        h.begin(sel(0, 1));
        h.begin(sel(0, 5));
        assert!(h.record(lines(&["ab"]), sel(0, 2)));
        assert!(h.record(lines(&["ab", "c"]), sel(1, 1)));
        let r = h.undo().unwrap();
        assert_eq!(r.lines, lines(&["ab"]));
        assert_eq!(r.sel, sel(0, 2), "no begin: falls back to previous step's selection");
        let r = h.undo().unwrap();
        assert_eq!(r.lines, lines(&["a"]));
        assert_eq!(r.sel, sel(0, 1), "first begin wins");
        assert!(h.undo().is_none());
        let r = h.redo().unwrap();
        assert_eq!((r.lines, r.sel), (lines(&["ab"]), sel(0, 2)));

        // New edit discards redo
        assert!(h.can_redo());
        assert!(h.record(lines(&["abx"]), sel(0, 3)));
        assert!(!h.can_redo());
        assert_eq!(h.undo().unwrap().lines, lines(&["ab"]));
    }

    #[test]
    fn limit() {
        let mut h = History::new(lines(&["0"]), None);
        for i in 1 ..= MAX_STEPS + 10 {
            h.record(lines(&[&i.to_string()]), None);
        }
        let mut undos = 0;
        while h.undo().is_some() {
            undos += 1;
        }
        assert_eq!(undos, MAX_STEPS - 1);
    }

    #[test]
    fn sharing() {
        let prev = share(&[], lines(&["a", "b", "c", "d"]));
        let next = share(&prev, lines(&["a", "x", "y", "c", "d"]));
        assert!(Rc::ptr_eq(&prev[0], &next[0]));
        assert!(Rc::ptr_eq(&prev[2], &next[3]));
        assert!(Rc::ptr_eq(&prev[3], &next[4]));
        assert_eq!(next.iter().map(|x| x.to_string()).collect::<Vec<_>>(), lines(&["a", "x", "y", "c", "d"]));
        // Overlapping prefix/suffix candidates
        let next = share(&prev, lines(&["a", "a"]));
        assert_eq!(next.iter().map(|x| x.to_string()).collect::<Vec<_>>(), lines(&["a", "a"]));
    }
}
