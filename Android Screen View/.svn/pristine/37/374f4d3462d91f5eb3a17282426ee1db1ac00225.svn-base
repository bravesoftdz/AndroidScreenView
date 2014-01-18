Really simple app. It works by calling the Android Debug Bridge (adb) which is 
part of the Android SDK. It takes a screenshot, coppies it locally, then 
deletes it.

adb -d shell screencap -p /sdcard/androidscreen.png
adb -d pull /sdcard/androidscreen.png [local path]
adb -d shell rm /sdcard/androidscreen.png 

It does this with a batchfile created dynamicly in the temp folder. 

After execution it displays the png and runs again.


TODO:
-----
Enumerate "adb devices" to make sure there is a device connected.
Allow user to choose device if more than one is connected.
Add command-line code to work on Mac toos