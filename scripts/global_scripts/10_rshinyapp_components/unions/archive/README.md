# Union Component Pattern

## Overview

The Union Component Pattern implements a compositional approach to building Shiny interfaces by combining multiple components into a unified whole. It follows several key principles:

1. **MP56 Connected Component Principle**: Components return parts that can be connected separately
2. **MP52 Unidirectional Data Flow**: Data flows in one direction
3. **MP54 UI-Server Correspondence**: UI elements have corresponding server-side functionality 
4. **R09 UI-Server-Defaults Triple Rule**: Components maintain separation of UI, server, and defaults
5. **P62 Separation of Concerns**: Each component focuses on its specific functionality

## Key Files

- `Union.R`: Main implementation of the Union pattern
- `Union.md`: Documentation of the pattern and usage examples
- `CompleteUnion_test.R`: Comprehensive example of using Union with tabbed navigation

## Usage

The Union pattern can be used in three ways:

1. **Direct Union**: Combine existing component instances
```r
union <- Union(
  "my_union",
  comp1 = component1,
  comp2 = component2,
  config = list(
    initial_visibility = list(
      comp1 = TRUE,
      comp2 = FALSE
    )
  )
)
```

2. **Union Component**: Create a pre-initialized union with UI and server functions
```r
union_comp <- UnionComponent(
  "my_union",
  components = list(
    comp1 = component1,
    comp2 = component2
  ),
  app_data_connection = data_conn,
  config = config_list
)
```

3. **Create Union**: Create a union directly from component creator functions
```r
union_comp <- CreateUnion(
  "my_union",
  component_creators = list(
    comp1 = comp1Creator,
    comp2 = comp2Creator
  ),
  app_data_connection = data_conn,
  config = union_config,
  component_configs = list(
    comp1 = comp1_config,
    comp2 = comp2_config
  )
)
```

## Component Structure Requirements

For a component to be compatible with Union, it must follow the structure defined in RC03_app_component_template.md:

```r
component <- list(
  ui = list(
    filter = function(id) { ... },
    display = function(id) { ... }
  ),
  server = function(input, output, session) { ... },
  defaults = function() { ... }  # Optional but recommended
)
```

The `defaults` function is optional but recommended for consistency and to enable automatic merging of default values from all components.

## Returns

The Union pattern returns a component with the same structure as its inputs, plus some additional metadata:

```r
list(
  ui = list(
    filter = function(id) { ... },
    display = function(id) { ... }
  ),
  server = function(id, app_data_connection, session) { ... },
  defaults = function() { ... },
  components = list(...)  # Metadata about included components
)
```

## Visibility Control

One of the key features of the Union pattern is visibility control. Components in a union can be shown or hidden dynamically:

```r
# In server function
observeEvent(input$show_comp2, {
  union_server$component_state$toggle_component("comp2", TRUE)
  union_server$component_state$toggle_component("comp1", FALSE)
})
```

## Principles Followed

This implementation follows:

1. **MP56 Connected Component Principle**: Components return parts that can be connected separately
2. **MP52 Unidirectional Data Flow**: Data flows in one direction from input to display 
3. **MP54 UI-Server Correspondence**: UI elements have corresponding server-side functionality
4. **R88 Shiny Module ID Handling**: Proper namespace management across components
5. **R91 Universal Data Access Pattern**: Components access data consistently
6. **P81 Tidyverse-Shiny Terminology Alignment**: Consistent terminology
7. **P62 Separation of Concerns**: Clear separation between UI, server, and defaults
8. **R09 UI-Server-Defaults Triple Rule**: Components maintain separation of UI, server, and defaults
