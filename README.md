# Torad
Torad is an Autolisp application for Autocad for exporting geometry data to the
<a href="http://www.radiance-online.org/">Radiance</a> lighting simulation package.

<i>Note: This is here mainly for historical purposes (created 1993). Please consider using <a href="/en/download/radout/">radout</a> instead. </i>

Basic features of torad are:

<ul TYPE=SQUARE>
<li> Makes complete translation of all hiding/shading entities from
    Autocad (and some more). 
<li> Entities can be selected on screen for partial export.
<li> Entities will be sorted by one of the three criteria color, layer or
    insertion layer of blocks (toplayer) according to your choice.
    The sorting results in seperate files written for every layer or color.
    Floating layers and colors within blocks are fully supported.
<li> Other files created optionally can contain initial material definitions
    (all identical), a list of "!cat" commands to include all the
    information to make up a complete scene description, a setup of natural
    lighting, a view description and a makefile for automatic image creation.
<li> Only the entities that are visible (that is their layer is on and thawed)
    will be exported, even when nested in a selected block. This gives you
    another method of filtering elements of your drawing especially usefull
    when you want to update only part of a scene.
<li> The layername or color number will be part of each respective filename
    to make it an unique identifier.
<li> You have full control through a sreen menu section, a text dialog
    or even a interactive dialog box if you are running Autocad 12 (or newer).
</ul>

More details on the <a href="http://www.schorsch.com/en/download/torad/">Torad Homepage</a>
