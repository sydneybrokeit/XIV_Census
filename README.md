# XIV_Census

This is a script to generate images from a dump of the FFXIV Lodestone using @pricetx's script.  When the script is completed, convert it to .csv (using SQLite Browser, for example) and then put that filename into the PlayerSetName variable.  You may need to change the file name.

As it stands now, it generates:

* Level profile graphs (overall, 50+, and 60)
* Faceted level profile graphs (overall , 50+, and 60)
* Cross-Class skill graphs (only for those with the primary class at 60)
* Demographics graphs (overall, 50+, and 60) [Server population by gender, server population by Grand Company, and races broken down by gender)

It's not pretty, but it works, and the entire process takes approximately an hour.

#TODO

* Make code more modular
* Optimize for faster running
* Pull out player IDs and write them to a file (to be used with @pricetx's script)
* Additional data as requested
* Create representation of server changes