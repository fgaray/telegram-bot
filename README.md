# Private Telegram  Bot

This is a bot for a private chat room in Telegram using Haskell. The idea is to
create a fun set of command that my friends and I can enjoy. This is not meant
to be use by other people, but if you want to host a private version of this bot
you are welcome to do so.



# Compile

You need stack to compile this bot. Use:

```
stack build
```

To build the bot.


# How to run

You need an API key for a telegram bot. Then just run:

```
TOKEN=botBOTID:YOUR-API-KEY ./los-programadores-bot
```

Remember to put "bot" in your API KEY like "bot123:abcd"

# Set of commands

## /dbsize

Shows the number of total messages in the DB")

## /dbstats

Show the number of messages per user

## /markov [USER]

Runs a markov chain of the user previous messages to generate a random string.

## /markovtrain [USER]

Trains a markov chain using the messages of the [USER]

## /last [USER]

Show the last message of the [USER]

## /bestof

Show the message of the week that is followed by the highest number of messages
in 60 seconds.


## /topreplies

Show the message of the week with the highest number of replies.



# New actions

If you want to add new actions that respond to messages, you can add them in
src/Actions.hs with a POSIX regex.
