pub mod declaration;
pub mod level;
pub mod term;

use kernel::memory::arena::Arena;

/// The trait of types that can be built into an arena-dependent element.
pub trait Buildable<'build> {
    /// The closure used to build the [`Output`] element.
    ///
    /// Typically a [`BuilderTrait`].
    ///
    /// [`BuilderTrait`]: term::builder::BuilderTrait
    /// [`Output`]: Buildable::Output
    type Closure;

    /// The corresponding element to build.
    type Output<'arena>;

    /// Realise a builder into a [`Output`].
    ///
    /// # Errors
    /// If the [`Output`] could not be built, yields an error indicating the reason.
    ///
    /// [`Output`]: Buildable::Output
    fn realise<'arena>(&self, arena: &mut Arena<'arena>) -> Result<'arena, Self::Output<'arena>>;

    /// Associates a builder to a builder trait.
    fn as_closure(&'build self) -> Self::Closure;
}
