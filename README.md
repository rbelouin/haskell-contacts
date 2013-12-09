Birthday Reminder
-----------------

## Intro

This is a little web API to help you remind your friends birthday.
Once running, you just have to `GET /contacts` to get your whole friends list or `GET /contacts?days=4` to get the list of the friends that have birthday in the next four days.

## How to build the project

### Sandbox init

    cabal sandbox init

### Dependencies installation

    cabal install HDBC HDBC-postgresql blaze-builder http-types json wai warp

### Build

    cabal build

## Run the project

    ./dist/build/Birthdays/Birthdays  # listen on 8080

## Profit.
