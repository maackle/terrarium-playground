# Introduction to terrarium

## Design decisions

### Ports as "first-class citizens"

While Blocks are the main attraction, Ports (inputs and outputs) are actually what get connected together to make the ecosystem. Early in design I had planned to include a list of Ports directly on the Block to which they belong, but this became cumbersome for a few reasons:

- defined this way, Ports are 3 layers deep
- it is most convenient to be able to use Ports directly as graph nodes (i.e. to be able to use them as keys in a hashmap). To ensure their uniqueness, it's important to have a `:block` property directly on the Port, to distinguish each port from otherwise similar ones. (Clojure treats structurally equal objects as equal)

### Lightweight in/out Ports

Ports are dead-simple. They comprise a text description of what goes in or out, and how much of it. It has no concept of substance, or its properties. Instead, we have Accounts, which can have a richer notion of what the substance being exchanged is. Any input-output connection has an Account as a middleman. For instance:

- block O has "poopy water" as an output
- block I has "fertilizer" as an input
- we have a "blackwater" account

In this example, we can hook up "poopy water" to "fertilizer" via "blackwater". Rather than the system determining whether or not this is a reasonable match, the user asserts that it is so by making this connection. As the program runs, the "blackwater" account increases by O's output rate and decreases by I's input rate.