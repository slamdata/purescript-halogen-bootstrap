## Module Halogen.Themes.Bootstrap3.InputGroup

This module provides convenience functions for creating _input groups_.

#### `AddOn`

``` purescript
data AddOn p i
  = RegularAddOn (HTML p i)
  | ButtonAddOn (HTML p i)
```

Represents an input group add-on element

We need to distinguish buttons from regular add-ons because of the
different CSS classes

#### `inputGroup`

``` purescript
inputGroup :: forall p i. Maybe (AddOn p i) -> HTML p i -> Maybe (AddOn p i) -> HTML p i
```

Create an input group.

An input group consists of a control with optional elements placed before and after.


