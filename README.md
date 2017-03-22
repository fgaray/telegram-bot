# Private Telegram  Bot

This is a bot for a private chat room in Telegram using Haskell. The current
state is very simply: it just store the messages in a sqlite database and
respond to messages with matching regex.


# Compile

You need stack to compile this bot. Use:


```
stack build
```

To build the bot.


# How to run

You need an API key for a telegram bot. Then just run:

```
TOKEN=YOUR-API-KEY ./los-programadores-bot
```

# New actions

If you want to add new actions that respond to messages, you can add them in
src/Actions.hs with a POSIX regex.
