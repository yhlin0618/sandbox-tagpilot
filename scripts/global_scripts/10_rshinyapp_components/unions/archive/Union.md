# Union Component Pattern

## Overview

The Union Component Pattern is a design approach for creating modular, composable applications that allow multiple micro-components to be combined into a cohesive user interface. This pattern implements several key principles:

- **MP56 Connected Component Principle**: Components return parts that can be connected separately
- **MP52 Unidirectional Data Flow**: Data flows in one direction from selection to visibility
- **MP54 UI-Server Correspondence**: UI elements have corresponding server-side functionality
- **R88 Shiny Module ID Handling**: Proper namespace management across components
- **R91 Universal Data Access Pattern**: Components access data consistently

## Pattern Structure

The Union Component Pattern consists of three main parts:

1. **Component Creators**: Functions that create individual micro-components
2. **Union Functions**: A family of functions to combine components into a unified interface
3. **Controller**: Manages component visibility and state

## Component Requirements

For a component to work with the Union pattern, it must adhere to the standard component template defined in RC03.

Components must:

1. Return a list with the following structure:
   ```r
   list(
     ui = list(
       filter = function(id) { ... },  # UI for filters in sidebar
       display = function(id) { ... }  # UI for main display area
     ),
     server = function(input, output, session) { ... },
     defaults = function() { ... }  # Optional default values
   )
   ```

2. Follow proper module ID handling (R88)
3. Use consistent data access patterns (R91)

For detailed component implementation requirements, refer to:
[RC03: App Component Template](/update_scripts/global_scripts/00_principles/RC03_app_component_template.md)

## Union Functions

The Union pattern includes a family of functions to create and manage combined components:

### 1. `Union` Function

The core factory function that combines multiple component instances:

```r
# Union combines existing component instances
Union(id, component1 = comp1, component2 = comp2, config = NULL)
```

**Parameters**:
- **id**: Base ID for the union component
- **...**: Named component instances to include in the union
- **config**: Configuration for the union

**Example**:
```r
# Create components first
customer_comp <- customerComponent("customer", data_conn)
product_comp <- productComponent("product", data_conn)

# Then combine them with Union
my_union <- Union(
  "my_union",
  customer = customer_comp,
  product = product_comp,
  config = list(
    initial_visibility = list(
      customer = TRUE,
      product = FALSE
    )
  )
)
```

### 2. `UnionComponent` Function

An initializer that creates a fully set up union component:

```r
# UnionComponent takes a list of component instances
UnionComponent(id, components, app_data_connection = NULL, config = NULL)
```

**Parameters**:
- **id**: ID for the union component
- **components**: Named list of component instances
- **app_data_connection**: Data connection to pass to server functions
- **config**: Configuration options

**Example**:
```r
components <- list(
  customer = customerComponent("customer", data_conn),
  product = productComponent("product", data_conn)
)

my_union <- UnionComponent("my_union", components, data_conn, config)
```

### 3. `CreateUnion` Function

A convenience function that creates components from creator functions:

```r
# CreateUnion takes component creator functions and handles setup
CreateUnion(
  id, 
  component_creators, 
  app_data_connection = NULL, 
  config = NULL, 
  component_configs = NULL
)
```

**Parameters**:
- **id**: ID for the union component
- **component_creators**: Named list of component creator functions
- **app_data_connection**: Data connection for component creation
- **config**: Union configuration
- **component_configs**: Individual configurations for each component

**Example**:
```r
my_union <- CreateUnion(
  "my_union",
  component_creators = list(
    customer = customerComponent,
    product = productComponent
  ),
  app_data_connection = data_conn,
  config = list(
    initial_visibility = list(
      customer = TRUE,
      product = FALSE
    )
  ),
  component_configs = list(
    customer = list(show_details = TRUE),
    product = list(category_filter = TRUE)
  )
)
```

### Configuration Options

The `config` parameter can include:

```r
list(
  initial_visibility = list(
    component1 = TRUE,   # Component initially visible
    component2 = FALSE   # Component initially hidden
  ),
  theme = list(...),     # Theme settings
  layout = "navbar",     # Layout type
  other_settings = ...   # Additional settings
)
```

### Return Value

The Union function returns a complete component with:

```r
list(
  ui = list(
    filter = function(id) { ... },  # Combined filters UI
    display = function(id) { ... }  # Combined display UI
  ),
  server = function(input, output, session) {
    # Returns state management and server results
    list(
      component_state = reactiveValues(...),
      server_outputs = list(...)
    )
  },
  defaults = function() { ... },  # Combined defaults from all components
  components = list(...)  # Metadata about components
)
```

## Composite Design Pattern

A key feature of the Union pattern is that it implements the **Composite Design Pattern**. This means:

1. The output of the Union function follows the same structure as individual components
2. A union can be used anywhere a component is expected
3. Unions can be nested within other unions

### Component vs Union Structure

**Component Output:**
```r
component <- microCustomerComponent(id, data_connection, config)
# Returns:
list(
  ui = list(
    filter = function(id) { ... },
    display = function(id) { ... }
  ),
  server = function(input, output, session) { ... },
  defaults = function() { ... }  # Optional default values
)
```

**Union Output:**
```r
union <- Union(id, component1, component2, config)
# Returns:
list(
  ui = list(
    filter = function(id) { ... },  # Combined filters
    display = function(id) { ... }  # Combined displays
  ),
  server = function(input, output, session) { ... },  # Combined servers
  defaults = function() { ... },  # Combined defaults from all components
  components = list(...)  # Metadata about components
)
```

This consistent interface allows for powerful compositional patterns:

```r
# Create a union of two components
small_union <- Union("small", componentA, componentB)

# Use that union as a component in a larger union
large_union <- Union("large", small_union, componentC)
```

The Union function implements a combined defaults function that merges the defaults from all components. The server function also provides additional state management capabilities, but the basic interface structure remains the same.

## Implementation Patterns

The Union pattern can be implemented in multiple ways depending on your needs:

### Approach 1: Direct Use of `Union`

Best when you need maximum control and have already created your components:

```r
# 1. Create Individual Components
customer_comp <- microCustomerComponent(
  id = "customer", 
  data_connection,
  config = customer_config
)

dna_comp <- microDNADistributionComponent(
  id = "dna",
  data_connection,
  config = dna_config
)

# 2. Configure Union
union_config <- list(
  initial_visibility = list(
    customer = TRUE,
    dna = FALSE
  )
)

# 3. Create Union
union <- Union(
  "main_union",
  customer = customer_comp,
  dna = dna_comp,
  config = union_config
)

# 4. Use in UI
ui <- dashboardPage(
  sidebar = dashboardSidebar(
    union$ui$filter("main_union")
  ),
  body = dashboardBody(
    union$ui$display("main_union")
  )
)

# 5. Use in Server
server <- function(input, output, session) {
  union_server <- union$server("main_union", data_connection, session)
  
  # Control visibility
  observeEvent(input$show_dna, {
    union_server$component_state$toggle_component("dna", TRUE)
    union_server$component_state$toggle_component("customer", FALSE)
  })
}
```

### Approach 2: Using `UnionComponent`

Best when you have a list of components and want a simpler interface:

```r
# 1. Create Components and Put in List
components <- list(
  customer = microCustomerComponent("customer", data_connection),
  dna = microDNADistributionComponent("dna", data_connection)
)

# 2. Create Union with UnionComponent
union <- UnionComponent(
  "main_union",
  components,
  data_connection,
  config = list(
    initial_visibility = list(
      customer = TRUE,
      dna = FALSE
    )
  )
)

# 3. Use in App
ui <- dashboardPage(
  sidebar = dashboardSidebar(
    union$ui$filter
  ),
  body = dashboardBody(
    union$ui$display
  )
)

server <- function(input, output, session) {
  union_server <- union$server(input, output, session)
  
  # Control visibility
  observeEvent(input$toggle_view, {
    current_view <- input$toggle_view
    union_server$component_state$toggle_component("customer", current_view == "customer")
    union_server$component_state$toggle_component("dna", current_view == "dna")
  })
}
```

### Approach 3: Using `CreateUnion`

Best for creating unions directly from component creator functions:

```r
# 1. Define Component Configs
component_configs <- list(
  customer = list(show_details = TRUE),
  dna = list(show_chart = TRUE)
)

# 2. Create Union with CreateUnion
union <- CreateUnion(
  "main_union",
  component_creators = list(
    customer = microCustomerComponent,
    dna = microDNADistributionComponent
  ),
  app_data_connection = data_connection,
  config = list(
    initial_visibility = list(
      customer = TRUE,
      dna = FALSE
    )
  ),
  component_configs = component_configs
)

# 3. Use the same way as Approach 2
ui <- dashboardPage(...)
server <- function(input, output, session) {...}
```

## Navigation Options

The Union pattern supports multiple navigation approaches:

### 1. Sidebar Menu

```r
bs4Dash::sidebarMenu(
  id = "navbar_menu",
  bs4Dash::menuItem(
    text = "Customer Analysis",
    tabName = "customer",
    icon = icon("user")
  ),
  bs4Dash::menuItem(
    text = "DNA Distribution",
    tabName = "dna",
    icon = icon("dna")
  )
)
```

### 2. Tab Navigation

```r
bs4Dash::tabItems(
  bs4Dash::tabItem(
    tabName = "customer",
    # Customer display
  ),
  bs4Dash::tabItem(
    tabName = "dna",
    # DNA display
  )
)
```

### 3. Navbar Menu

```r
bs4Dash::bs4DashNavbar(
  # ...
  rightUi = tagList(
    bs4Dash::bs4DropdownMenu(
      # Component selection
    )
  )
)
```

## Data Flow

The Union pattern implements unidirectional data flow:

1. **User Action**: User selects a component to view
2. **State Change**: The active component is updated in reactive state
3. **Visibility Update**: Component visibility is updated based on state
4. **Filter Update**: Corresponding filters are shown/hidden
5. **Display Update**: Component displays are shown/hidden

This is documented using NSQL syntax:

```
FROM INPUT.component_selection
SELECT component_name
STORE IN reactive::active_component;

FROM reactive::active_component
JOIN WITH (SELECT all_components FROM union::components)
WHERE component_name = active_component
UPDATE visibility = TRUE
UPDATE visibility = FALSE WHERE component_name != active_component;

FROM filter_components
JOIN WITH reactive::active_component
UPDATE filters.visibility = TRUE WHERE component_name = active_component
UPDATE filters.visibility = FALSE WHERE component_name != active_component;
```

## Example Usage

See the `Union_test.R` and `CompleteUnion_test.R` files for complete examples of implementing the Union Component Pattern.

## Component State Management

A key feature of the Union pattern is the state management for component visibility:

```r
# Access the state management in server function
union_server <- union$server(id, app_data_connection, session)

# Functions available in component_state:
union_server$component_state$toggle_component("component_name", TRUE)  # Show component
union_server$component_state$toggle_component("component_name", FALSE) # Hide component
union_server$component_state$toggle_component("component_name")        # Toggle visibility
union_server$component_state$is_visible("component_name")              # Check visibility
union_server$component_state$get_visible_components()                  # Get all visible components

# Accessing server outputs from components
customer_data <- union_server$server_outputs$customer
dna_data <- union_server$server_outputs$dna
```

## Best Practices

1. **Naming & Organization**
   - Always provide unique names for components in the union
   - Use descriptive component names related to their function
   - Group related components into logical unions

2. **Structure & Design**
   - Ensure all components follow the required UI-Server-Defaults structure
   - Use consistent styling across components (consider sharing CSS)
   - Keep components focused on single responsibilities

3. **Performance Considerations**
   - Consider performance implications of mounting many components
   - Consider lazy-loading components that aren't initially visible
   - Monitor reactive dependencies between components

4. **Usage Patterns**
   - Choose the appropriate Union function for your needs:
     - `Union`: When you need maximum control with existing components
     - `UnionComponent`: When you have a list of components
     - `CreateUnion`: When working with component creator functions

5. **Component Coupling**
   - Design components to be independent when possible
   - Use the defaults function for component configuration
   - Be mindful of data sharing between components in a union

6. **Visibility Management**
   - Control component visibility through the state management API
   - Consider using navigation patterns that match your UI design 
   - Set appropriate initial visibility for components