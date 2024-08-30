#[doc(hidden)]
pub mod __exports {
    pub use paste;
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

                    $vis fn [<visit_ $suffix:snake>]<V : $id + ?Sized>(&self, $visitor_id: &mut V, $($visited_id: $visited),*) -> std::result::Result<(), V::Err> {
                        $block
                    }

                )*
            }


        }


        $(#[$attr])*
        $vis trait $id {
            type Err;

            $crate::__exports::paste::paste! {
                fn defaults(&self) -> [<$id:camel Functions>] {
                    [<$id:camel Functions>]
                }
            }

            $(
            $crate::__exports::paste::paste! {
                fn [<visit_ $suffix:snake>](&mut self, $($visited_id: $visited),*) -> std::result::Result<(), Self::Err> {
                    [<$id:camel Functions>].[<visit_ $suffix:snake>](self, $($visited_id),*)
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
];

#[cfg(test)]
mod tests {
    use crate::visitor;

    visitor! {
        pub trait Simple {}
    }

    #[test]
    #[ignore]
    fn _test_dummy() {}
}
