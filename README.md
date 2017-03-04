# Los Programadores Bot

This is a bot for a private chat room in Telegram using Haskell. The current
state is very simply: it just store the messages in a sqlite databse and execute
an awk program for matching strings.


We are using an awk program so anybody in the chat can add their own simple bot
responses in messages.awk.
