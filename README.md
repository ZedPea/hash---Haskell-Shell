# hash - haskell-shell
A very simple shell written in haskell.

## Inbuilt commands:

help -- displays a help command then exits

exit -- exit the shell

cd -- change directory - defaults to home directory if no argument given

alias -- display aliases found in config file

## Config file:

must be located in ~/.hashrc

Set the prompt with

`prompt="newprompt> "`

You can use double or single quotes.

Set aliases with

`alias foo="bar"`

You can use double or single quotes.
