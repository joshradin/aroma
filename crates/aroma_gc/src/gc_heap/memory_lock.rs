use std::marker::PhantomData;
use parking_lot::Mutex;
use std::sync::{Arc, Weak};
use std::thread::yield_now;

#[derive(Debug, Default)]
struct LockState {
    /// counts the number of objects current accessing memory
    accessing_memory: usize,
    /// If locked, can not increment
    locked: usize,
}

/// Used for restricting access to the memory,
#[derive(Debug, Clone)]
pub struct MemoryLock {
    /// a guard
    guard: Arc<Mutex<LockState>>,
}

#[derive(Debug, Clone)]
pub struct WeakMemoryLock(Weak<Mutex<LockState>>);

impl WeakMemoryLock {
    /// Upgrade to a MoveGuard
    pub fn upgrade(&self) -> Option<MemoryLock> {
        self.0.upgrade().map(|s| MemoryLock { guard: s })
    }
}

impl MemoryLock {
    /// Creates a new move guard
    pub fn new() -> Self {
        Self {
            guard: Default::default(),
        }
    }

    /// Creates a weak version
    pub fn weak(self: &Self) -> WeakMemoryLock {
        WeakMemoryLock(Arc::downgrade(&self.guard))
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
    pub fn lock(&self) -> MemoryGuard {
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
        MemoryGuard { lock: self.clone(), _lf: PhantomData }
    }

    /// Tries to lock any increment request coming in, forcing any inc to wait. If any accesses currently
    /// exist this will fail.
    /// Each lock call must be accompanied by an unlock call. Lock waits until no more memory accesses.
    pub fn try_lock(&self) -> Result<MemoryGuard, MoveGuardLockError> {
        let mut guard = self.guard.lock();
        if guard.accessing_memory > 0 {
            Err(MoveGuardLockError(guard.accessing_memory))
        } else {
            guard.locked += 1;
            Ok(MemoryGuard {
                lock: self.clone(),
                _lf: Default::default(),
            })
        }
    }

    /// Checks if there currently is a lock on the move guard
    pub fn is_locked(&self) -> bool {
        self.guard.lock().locked > 0
    }

    /// Unlocks this guard, allowing for more inc calls.
    fn unlock(&self) {
        let mut guard = self.guard.lock();
        if guard.locked > 0 {
            guard.locked -= 1;
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("Can't lock because {0} accesses exist")]
pub struct MoveGuardLockError(usize);

/// A guard that's produced by a [MemoryLock].
///
/// While a guard exists, the [MemoryLock::inc] methods becomes a blocking method.
#[derive(Debug)]
pub struct MemoryGuard<'a> {
    lock: MemoryLock,
    _lf: PhantomData<&'a ()>
}

impl Drop for MemoryGuard<'_> {
    fn drop(&mut self) {
        self.lock.unlock();
    }
}