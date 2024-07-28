use super::*;

/// Performs a merge sort on a linked list by starting at it's head
pub unsafe fn merge_sort<T, F>(head: Link<T>, compare: &mut F) -> Link<T>
where
    F: FnMut(&T, &T) -> Ordering,
{
    head.and_then(|head| {
        let next = (*head.as_ptr()).next;
        next.and(Some(head))
    })
    .and_then(|head| {
        let (mut first, mut second) = split(Some(head));
        first = merge_sort(first, compare);
        second = merge_sort(second, compare);

        merge(first, second, compare)
    })
    .or(head)
}

/// merge too lists together
unsafe fn merge<T, F>(first: Link<T>, second: Link<T>, compare: &mut F) -> Link<T>
where
    F: FnMut(&T, &T) -> Ordering,
{
    match (first, second) {
        (None, None) => None,
        (first @ Some(_), None) => first,
        (None, second @ Some(_)) => second,
        (Some(first), Some(second)) => {
            if compare(&(*first.as_ptr()).elem, &(*second.as_ptr()).elem) == Ordering::Less {
                (*first.as_ptr()).next = merge((*first.as_ptr()).next, Some(second), compare);
                if let Some(next) = (*first.as_ptr()).next {
                    (*next.as_ptr()).prev = Some(first);
                }
                (*first.as_ptr()).prev = None;
                Some(first)
            } else {
                (*second.as_ptr()).next = merge(Some(first), (*second.as_ptr()).next, compare);
                if let Some(next) = (*second.as_ptr()).next {
                    (*next.as_ptr()).prev = Some(second);
                }
                (*second.as_ptr()).prev = None;
                Some(second)
            }
        }
    }
}

unsafe fn len<T>(mut link: Link<T>) -> usize {
    let mut length = 0;
    while let Some(ptr) = link {
        link = (*ptr.as_ptr()).next;
        length += 1;
    }
    length
}

unsafe fn split<T>(head: Link<T>) -> (Link<T>, Link<T>) {
    if let Some(head) = head {
        let mut fast = head;
        let mut slow = head;

        while let (Some(next_slow), Some(next_fast)) = (
            (*fast.as_ptr()).next,
            (*fast.as_ptr()).next.and_then(|next| (*next.as_ptr()).next),
        ) {
            fast = next_fast;
            slow = next_slow;
        }
        let temp = (*slow.as_ptr()).next;
        if let Some(temp) = temp {
            (*temp.as_ptr()).prev = None;
        }
        (*slow.as_ptr()).next = None;
        (Some(head), temp)
    } else {
        (None, None)
    }
}
