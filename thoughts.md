I'm not sure what's best here. I think there's two separate issues: 


# Channels

This is potentially broken and should be fixed. 

Check whether swap_test2 executes both. 

# Paired composition

I understand that ideally paired composition would be as parallel (or as eager) as possible. That is, if there if there is any order of executing reagents that will let them terminate, it should be able to find it. 

It's not as simple as the <+>, since we have to ensure we don't commit just one reagent (maybe pass an arg to the commit?). We also need to submit separate offers, as they have to be able to 


# README

Describe the benefits, show examples and core benchmarks 


passing variable with destruction