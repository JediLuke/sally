sally
=====

NOTE: Will compile but not run at the moment :(

My ongoing side project to develop a "black box" machine learning platform that trains both weightings and hyper-parameters of neural networks.

Inspired by DXNN2 - https://github.com/CorticalComputer/DXNN2

I am to a) simplify the structure of the code, b) add other optimization algorithms such as PSO (already working) and c) release a version that compiles without any hassle to the world (so far, neither of us have succeeded at this...)

Sally - Because if you're going to build something that's potentially an increibly powerful AI, at least name it after a beautiful soul :)


Issues
-----

* Will compile but not run at the moment
* Tabs/spaces have been screwed up when uploading to git. If you open with sublime text using the workspace file provided, it should be all good.



Build
-----

    $ rebar3 shell


Run
-----

	$ application:start(sally)
	$ sally_app:start()
	$ sally_app:input(Artilect_Name, Module, Input)


## How this whole thing works

Artilects are spawned as various cortexs (controllers) and lobes (processors).

Thalamus: Responsible for I/O
Orbito: Decides which outputs from Lobes are good/best

Lobes
------
PSO: Particle swarm optimizer


Inputs: Inputs go to the Thalamus, where they are distributed to the lobes. Lobes return calculation to the Orbito,
which then instructs the Thalamus on outputs.


Flow of execution
-----------------

Program top level: Sally.erl is the application callback module. Here we can create new artilects, and give them input.

When the application is started, it builds a supervisor tree. This supervisor tree starts all the cortex and lobe supervisors.

Rather than build our own neural nets, I created a simple method called 'go' which gives us a test scenario. So, so far, to start the application you need to

$> rebar3 shell
$> application:start(sally).
$> sally:go().

The application callbacks are largely just external interfaces to artilects. Interacting with artilects (at least on the input) is done via artilect.erl API functions

When an artilect is created, the supervisors and lobes are all automatically started. The supervisors pretty much just exist to allow us to spawn new cortexes and lobes under them, at this point they're all simple_one_for_one (probably need to fix that at some point). Therefore, no surprises here, but the only function besides the ones required in supervisors is to spawn cortex/lobes under that particular supervisor. All other interaction is performed by message passing to the gen_server we've spawnwed

Thus interaction is performed simply by sending gen_server (which we've already created) messages. Each cortex/module has APIs at the top which abstract away this message passing.