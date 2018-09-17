# TirelessTracker

## Liam Frye-Mason

#### A small Haskell application that keeps track of MTG matches and provides statistics. 

-------

How to use:
----------

### Commands:

*help*                       - Displays help information

*add* [match]                - Adds [match] if it is in MATCHFORMAT

*show* [(optional) command]  - Displays the list of matches

    -d [deck name] - Only show matches played with [deck name]
    -o [deck name] - Only show matches played against [deck name]
    -t [year] [month] [day] - Only show matches played on the date
    -e [event name] - only show matches played in [event name]
*stats*                       - Show win loss and draw statistics for the total and all matches-ups 

*sort*                        - Sort the matches based on the command specifier

    -d - Sort matches by deck played
    -o - Sort matches by opponent deck 
    -t - Sort matches by date
    -e - Sort matches by event name

*load* [file name]            - Load a file from [file name] location

*save* [(optional)file name]  - saves matches at [file name] if specified, 
otherwise to the default location

*exit*                       - Exits Tireless Tracker

### Other:

MATCHFORMAT    - [deck name] [opponent deck name] [# of won games] [# of lost games] [year] [month as number] [day of month] [event name]
