# hash - haskell-shell
A very simple shell written in haskell.

Code is pretty rubbish, not in very good haskell style

## Inbuilt commands:

help -- displays a help command then exits

exit -- exit the shell

cd -- change directory - defaults to home directory if no argument given

alias -- display aliases found in config file

## Config file:

must be located in ~/.hashrc

Set the prompt with

`prompt='newprompt> '`

Set aliases with

`alias foo='bar'`

You need to have single quotes around the prompt/alias, as indicated in above examples.
