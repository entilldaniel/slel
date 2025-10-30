# SLEL

A tool to query the SL API for departures from either a preselected station or from a list of destinations.

This tool is only intended for metro stations so it will only query for that kind of transportation even though it shows 
other destinations (bus stops etc.). Unfortunately I haven't found a simple way to filter out metro-stations yet. 
In the usual case the user would know about the stop name however.

And in the future I might expand it to show all departures.

### Installation and Usage

Should be as simple as loading it in your init.el and then interactively (M-x) call **sl/show-selected** or **sl/select-and-show**.
Within the buffer you can press **g** to refresh and **q** to close the buffer.

You can use the build in customization to change the variable **sl/my-stations** to get a subset of stations that you're interested in.


### TODO
[x] Make the preselected list customizable.
[x] Close/bury buffer with 'q'

