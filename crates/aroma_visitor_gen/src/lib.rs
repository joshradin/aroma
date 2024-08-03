/// Creates a mut visitor
#[macro_export]
macro_rules! visit_mut {
    (
        $trait_vis:vis trait $id:ident {
            $($vis:vis visit ($visitor_id:ident, $visited_id:ident: &mut $visited:ty) -> Result<()> $block:block)+
        }
        ) => {
        $(
        paste::paste! {
            $vis fn [<visit_ $visited:snake  _mut>]<V : VisitorMut + ?Sized>($visitor_id: &mut V, $visited_id: &mut $visited) -> std::result::Result<(), V::Err> {
                $block
            }
        }
        )*

        $trait_vis trait $id {
            type Err;

            $(
            paste::paste! {
                fn [<visit_ $visited:snake  _mut>](&mut self, $visited_id: &mut $visited) -> std::result::Result<(), Self::Err> {
                    [<visit_ $visited:snake  _mut>](self, $visited_id)
                }
            }
            )*
        }
    };
}

/// Creates a visitor
#[macro_export]
macro_rules! visit {
    ($trait_vis:vis trait $id:ident { $($vis:vis visit ($visitor_id:ident, $visited_id:ident: &mut $visited:ty) -> Result<()> $block:block)+ }) => {
        $(
        paste::paste! {
            $vis fn [<visit_ $visited:snake  _mut>]<V : VisitorMut + ?Sized>($visitor_id: &mut V, $visited_id: &mut $visited) -> std::result::Result<(), V::Err> {
                $block
            }
        }
        )*

        $trait_vis trait $id {
            type Err;

            $(
            paste::paste! {
                fn [<visit_ $visited:snake  _mut>](&mut self, $visited_id: &mut $visited) -> std::result::Result<(), Self::Err> {
                    [<visit_ $visited:snake  _mut>](self, $visited_id)
                }
            }
            )*
        }
    };
}
