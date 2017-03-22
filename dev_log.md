Developer log
===============

# BUGS (Fix first!)



# TODO

TODO Number 1

NEXT NUMBER 1 - Spawn scapes for each test. Pass in {Input, ExpectedOutput} -> Score (o to 1 based on how well it worked). Then particles are responsible for collating the results.
THEN figure out how to calculate new neural net configurations based on gBest
THEN get the fucking NN part actually working hahahaha

Add C node integration - shoehorn into backprop

Need to go and make nice APIs for all the cast/calls - each module should have WELL DEFINED API which is the ONLY WAY it gets used

- Implement behaviours for all modules/lobes, so that functions are forced to be inherited
- Run dializer
- Put function specs over everything
- Need to add metrics to this whole thing....
- Now that Cowboy is in there, I should set up so that it has a nice web interface. Should build a chatbot :) and a Go engine
	Start with a TicTacToe engine...
	Want nice interfaces to see how the NN grows over time, what inputs look like, ability to see all the artilects, set fitness funcs etc.
- Need to do cluster scaling...
- in PSO, Make new_swarm really make X number of particles
- Add spec checking to all modules
- Scape tests: %% NOTE: These should all be tested in Parallel :S hahaha

# Issues

- I'm sure using erlbus isn't the most efficient way to do message passing, way more useless
messages get passed around than useful ones.
- Pretty sure that Orbito is gonna need some kind of syncronization - otherwise one input will trigger multiple outputs (1 from each lobe)
	Orbito needs to: ->
		-> Decide what type of input it is (or is this Thalamus??)
		-> Get input from various lobes
		-> Put a confidence rating on outputs from various lobes
		-> Finally, give Thalamus all it needs to take actions quickly
			-> For X input, use Y lobe
	Really, Inputs should go to both Orbito and Thalamus. Thalamus decides the output, Orbito uses the 
- Because of the shitty simple_one_for_one architecture, when things crash they don't get restarted. I need a better way to do this, such that when a restart happens, it also restarts with previous State (yeah...)
- Warning in Orbito: Line 94, Lobe (which is the lobe that the msg is coming from) is unused - probably should be, or can we ditch it?

# Notes

- I stared doing this thing where supervisors have a naming convention. L1 is the highest level
supervisor, L2s are below them etc. Need to finish implementing this...

# Ideas

- Need to set up devops. Continuous running with code reload
	- Push to dev, run tests, lint, etc.
	- Push to staging for production test
	- Push to master: Do code reload on live server
	- Set up pipeline to deploy to x nodes, automatically spin up new nodes as needed



# record.log


22/10/2016 FIXED
- Need to use proper state variables (map? that have name & other useful info) (artilect, procedure & thalamus) - plus the rest...