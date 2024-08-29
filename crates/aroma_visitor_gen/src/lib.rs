#[doc(hidden)]
pub mod __exports {
    pub use paste;
}

#[doc(hidden)]
#[macro_export]
macro_rules! multi_visitors {
    (@declare-headers $(#[$attr:meta])* $vis:vis trait $id:ident ($($ty:ident $(: $ext:tt)?),+) { $($tail:tt)* }) => {
        $crate::multi_visitors!(
            @create-vistors $(
                (
                    $vis $id;
                    $ty $(: $ext)?;

                )
            ),*;
            $($tail)*
        );
    };
    (@create-vistors $(($vis:vis $id:ident; $ty:ident $(: $ext:tt)?; $( ($($visit_fn:tt)*) );*)),*;
        visit fn $suffix:ident ($visitor_id:ident, $visited_id:ident: $visited:ty) -> Result<()> $block:block $($tail:tt)* ) => {
        $crate::multi_visitors!(
            @create-visitors
            $(
                (
                    $vis $id;
                    $ty $(: $ext)?;
                    $( ($($visit_fn)*) ;)*
                    (visit fn $suffix ($visitor_id, $visited_id: $visited) -> Result<()> $block)
                )
            ),*;
            $($tail)*
        );
    };
    (@create-visitors $(($vis:vis $id:ident; $ty:ident $(: $ext:tt)?; $( ($($visit_fn:tt)*) );* )),*;) => {
        $crate::multi_visitors!(
            @create-completed-visitors
            ();
            $(
                ($vis $id; $ty $(: $ext)?;  $( ($($visit_fn)*) );*)
            )*
        );
    };
    (@create-completed-visitors ($($completed:item);*); ($vis:vis $id:ident; $ty:ident $(: $ext:tt)?; $( ($($visit_fn:tt)*) );*  ) $($tail:tt)*) => {
        $crate::multi_visitors!(
            @create-completed-visitors
            ($($completed;)* $crate::multi_visitors! { @create-completed-visitor $vis $id; $ty $(: $ext)?; $( ($($visit_fn)*) );*  });
            $($tail)*
        );
    };
    (@create-completed-visitors ($($completed:item);*); ) => {
        $(
            $completed
        )*
    };
    (@create-completed-visitor $vis:vis $id:ident; Ref;
    $( (
        visit fn $suffix:ident ($visitor_id:ident, $($visited_id:ident: $visited:ty),+) -> Result<()> $block:block
    ) );*
    ) => {
        $crate::__exports::paste::paste! {
            $vis struct [<$id:camel Ref Functions>];
            impl [<$id:camel Ref Functions>] {
                $(
                    $vis fn [<visit_ $suffix:snake>]<V : [<$id:camel Ref>] + ?Sized>($visitor_id: &mut V, $($visited_id: &$visited),*) -> std::result::Result<(), V::Err> {
                        $block
                    }
                )*
            }
            $vis trait [<$id:camel Ref>] {
                type Err;
                $(
                fn [<visit_ $suffix:snake>](&mut self, $($visited_id: &$visited),*) -> std::result::Result<(), Self::Err> {
                    [<$id:camel Ref Functions>]::[<visit_ $suffix:snake>](self, $($visited_id),*)
                }
                )*
            }
            impl<V : [<$id:camel Ref>]> [<$id:camel Ref>] for &mut V {
                type Err = V::Err;
                $(
                fn [<visit_ $suffix:snake>](&mut self, $($visited_id: &$visited),*) -> std::result::Result<(), Self::Err> {
                    (*self).[<visit_ $suffix:snake>]($($visited_id),*)
                }
                )*
            }
        }

    };
    (@create-completed-visitor $vis:vis $id:ident; Mut;
    $( (
        visit fn $suffix:ident ($visitor_id:ident, $($visited_id:ident: $visited:ty),+) -> Result<()> $block:block
    ) );*
    ) => {
        $crate::__exports::paste::paste! {
            $vis struct [<$id:camel Mut Functions>];
            impl [<$id:camel Mut Functions>] {
                $(
                    $vis fn [<visit_ $suffix:snake>]<V : [<$id:camel Mut>] + ?Sized>($visitor_id: &mut V, $($visited_id: &mut $visited),*) -> std::result::Result<(), V::Err> {
                        $block
                    }
                )*
            }
            $vis trait [<$id:camel Mut>] {
                type Err;
                $(
                fn [<visit_ $suffix:snake>](&mut self, $($visited_id: &mut $visited),*) -> std::result::Result<(), Self::Err> {
                    [<$id:camel Mut Functions>]::[<visit_ $suffix:snake>](self, $($visited_id),*)
                }
                )*
            }
            impl<V : [<$id:camel Mut>]> [<$id:camel Mut>] for &mut V {
                type Err = V::Err;
                $(
                fn [<visit_ $suffix:snake>](&mut self, $($visited_id: &mut $visited),*) -> std::result::Result<(), Self::Err> {
                    (*self).[<visit_ $suffix:snake>]($($visited_id),*)
                }
                )*
            }
        }
    };
    (@create-completed-visitor $vis:vis $id:ident; Mut: _;
    $( (
        visit fn $suffix:ident ($visitor_id:ident, $($visited_id:ident: $visited:ty),+) -> Result<()> $block:block
    ) );*
    ) => {
        $crate::__exports::paste::paste! {
            $vis struct [<$id:camel Functions>];
            impl [<$id:camel Functions>] {
                $(
                    $vis fn [<visit_ $suffix:snake>]<V : [<$id:camel>] + ?Sized>($visitor_id: &mut V, $($visited_id: &mut $visited),*) -> std::result::Result<(), V::Err> {
                        $block
                    }
                )*
            }
            $vis trait [<$id:camel>] {
                type Err;
                $(
                fn [<visit_ $suffix:snake>](&mut self, $($visited_id: &mut $visited),*) -> std::result::Result<(), Self::Err> {
                    [<$id:camel Functions>]::[<visit_ $suffix:snake>](self, $($visited_id),*)
                }
                )*
            }
            impl<V : [<$id:camel>]> [<$id:camel>] for &mut V {
                type Err = V::Err;
                $(
                fn [<visit_ $suffix:snake>](&mut self, $($visited_id: &mut $visited),*) -> std::result::Result<(), Self::Err> {
                    (*self).[<visit_ $suffix:snake>]($($visited_id),*)
                }
                )*
            }
        }
    };
    (@create-completed-visitor $vis:vis $id:ident; Ref: _;
    $( (
        visit fn $suffix:ident ($visitor_id:ident, $($visited_id:ident: $visited:ty),+) -> Result<()> $block:block
    ) );*
    ) => {
        $crate::__exports::paste::paste! {
            $vis struct [<$id:camel Functions>];
            impl [<$id:camel Functions>] {
                $(
                    $vis fn [<visit_ $suffix:snake>]<V : [<$id:camel>] + ?Sized>($visitor_id: &mut V, $($visited_id: &$visited),*) -> std::result::Result<(), V::Err> {
                        $block
                    }
                )*
            }
            $vis trait [<$id:camel>] {
                type Err;
                $(
                fn [<visit_ $suffix:snake>](&mut self, $($visited_id: &$visited),*) -> std::result::Result<(), Self::Err> {
                    [<$id:camel Functions>]::[<visit_ $suffix:snake>](self, $($visited_id),*)
                }
                )*
            }
            impl<V : [<$id:camel>]> [<$id:camel>] for &mut V {
                type Err = V::Err;
                $(
                fn [<visit_ $suffix:snake>](&mut self, $($visited_id: &$visited),*) -> std::result::Result<(), Self::Err> {
                    (*self).[<visit_ $suffix:snake>]($($visited_id),*)
                }
                )*
            }
        }
    };
}

/// Creates a visitor
#[macro_export]
macro_rules! visitor [
    (
        $(#[$attr:meta])*
        $vis:vis trait $id:ident {
            $(visit fn $suffix:ident ($visitor_id:ident, $($visited_id:ident: $visited:ty),+) -> Result<()> $block:block)*
        }
    ) => {
        $crate::__exports::paste::paste! {
            $vis struct [<$id:camel Functions>];

            impl [<$id:camel Functions>] {
                $(

                    $vis fn [<visit_ $suffix:snake>]<V : $id + ?Sized>($visitor_id: &mut V, $($visited_id: $visited),*) -> std::result::Result<(), V::Err> {
                        $block
                    }

                )*
            }
        }


        $(#[$attr])*
        $vis trait $id {
            type Err;

            $(
            $crate::__exports::paste::paste! {
                fn [<visit_ $suffix:snake>](&mut self, $($visited_id: $visited),*) -> std::result::Result<(), Self::Err> {
                    [<$id:camel Functions>]::[<visit_ $suffix:snake>](self, $($visited_id),*)
                }
            }
            )*
        }

        impl<V : $id> $id for &mut V {
            type Err = V::Err;

            $(
            $crate::__exports::paste::paste! {
                fn [<visit_ $suffix:snake>](&mut self, $($visited_id: $visited),*) -> std::result::Result<(), Self::Err> {
                    (*self).[<visit_ $suffix:snake>]($($visited_id),*)
                }
            }
            )*
        }
    };
    ($($tt:tt)*) => {
        $crate::multi_visitors! { @declare-headers $($tt)* }
    }
];

#[cfg(test)]
mod tests {
    use crate::visitor;

    visitor! {
        pub trait Simple {}
    }

    visitor! {
        pub trait Complex(Ref: _, Mut) {
            visit fn v(v, obj: i32) -> Result<()> {
                Ok(())
            }
        }
    }

    #[test]
    #[ignore]
    fn _test_dummy() {

    }
}