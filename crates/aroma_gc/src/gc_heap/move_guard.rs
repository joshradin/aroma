use parking_lot::Mutex;
use std::sync::{Arc, Weak};
use std::thread::yield_now;

#[derive(Debug, Default)]
pub struct GuardState {
    /// counts the number of objects current accessing memory
    accessing_memory: usize,
    /// If locked, can not increment
    locked: usize,
}

#[derive(Debug, Clone)]
pub struct MoveGuard {
    /// a guard
    guard: Arc<Mutex<GuardState>>,
}

#[derive(Debug, Clone)]
pub struct WeakMoveGuard(Weak<Mutex<GuardState>>);

impl WeakMoveGuard {
    /// Upgrade to a MoveGuard
    pub fn upgrade(&self) -> Option<MoveGuard> {
        self.0.upgrade().map(|s| MoveGuard { guard: s })
    }
}

impl MoveGuard {
    /// Creates a new move guard
    pub fn new() -> Self {
        Self {
            guard: Default::default(),
        }
    }

    /// Creates a weak version
    pub fn weak(self: &Self) -> WeakMoveGuard {
        WeakMoveGuard(Arc::downgrade(&self.guard))
    }

    /// Increment current accessing
    pub fn inc(&self) {
        loop {
            let mut guard = self.guard.lock();
            if guard.locked > 0 {
                yield_now();
            } else {
                guard.accessing_memory += 1;
                break;
            }
        }
    }

    /// Decrement current accessing
    pub fn dec(&self) {
        let mut guard = self.guard.lock();
        guard.accessing_memory -= 1;
    }

    /// Locks any increment request coming in, forcing any inc to wait.
    /// Each lock call must be accompanied by an unlock call. Lock waits until no more memory accesses.
    pub fn lock(&self) {
        let mut guard = self.guard.lock();
        guard.locked += 1;
        drop(guard);
        loop {
            let guard = self.guard.lock();
            if guard.accessing_memory > 0 {
                break;
            }
            yield_now();
        }
    }

    /// Tries to lock any increment request coming in, forcing any inc to wait. If any accesses currently
    /// exist this will fail.
    /// Each lock call must be accompanied by an unlock call. Lock waits until no more memory accesses.
    pub fn try_lock(&self) -> Result<(), MoveGuardLockError> {
        let mut guard = self.guard.lock();
        if guard.accessing_memory > 0 {
            Err(MoveGuardLockError(guard.accessing_memory))
        } else {
            guard.locked += 1;
            Ok(())
        }
    }

    /// Checks if there currently is a lock on the move guard
    pub fn is_locked(&self) -> bool {
        self.guard.lock().locked > 0
    }

    /// Unlocks this guard, allowing for more inc calls.
    pub fn unlock(&self) {
        let mut guard = self.guard.lock();
        if guard.locked > 0 {
            guard.locked -= 1;
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("Can't lock because {0} accesses exist")]
pub struct MoveGuardLockError(usize);
