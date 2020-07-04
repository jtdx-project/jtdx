#!/bin/sh

#
# Windows
#
#inkscape -z -e /tmp/image-0.png wsjt_globe_128x128.svg
#inkscape -z -e /tmp/image-1.png wsjtx_globe_1024x1024.svg
#convert '/tmp/image-%d.png[0-1]' -background transparent	\
#    \( -clone 0 -resize 16 -colors 256 -compress none \)	\
#    \( -clone 0 -resize 20 -colors 256 -compress none \)	\
#    \( -clone 0 -resize 24 -colors 256 -compress none \)	\
#    \( -clone 0 -resize 32 -colors 256 -compress none \)	\
#    \( -clone 0 -resize 40 -colors 256 -compress none \)	\
#    \( -clone 1 -resize 48 -colors 256 -compress none \)	\
#    \( -clone 1 -resize 96 -colors 256 -compress none \)	\
#    \( -clone 1 -resize 128 -colors 256 -compress none \)	\
#    \( -clone 0 -resize 16 -compress none \)	\
#    \( -clone 0 -resize 20 -compress none \)	\
#    \( -clone 0 -resize 24 -compress none \)	\
#    \( -clone 0 -resize 32 -compress none \)	\
#    \( -clone 0 -resize 40 -compress none \)	\
#    \( -clone 1 -resize 48 -compress none \)	\
#    \( -clone 1 -resize 64 -compress none \)	\
#    \( -clone 1 -resize 96 -compress none \)	\
#    \( -clone 1 -resize 128 -compress none \)	\
#    \( -clone 1 -resize 256 -compress Zip \)	\
#    -delete 1 -delete 0	\
#    -alpha remove ../icons/windows-icons/wsjtx.ico
#rm /tmp/image-0.png /tmp/image-1.png
#identify -format '%f %p/%n %m %C/%Q %r %G %A %z\n' ../icons/windows-icons/wsjtx.ico

inkscape -z -e /tmp/image-0.png jtdx_globe_128x128.svg
inkscape -z -e /tmp/image-1.png jtdx_globe_1024x1024.svg
convert '/tmp/image-%d.png[0-1]' -background transparent	\
    \( -clone 0 -resize 16 -colors 256 -compress none \)	\
    \( -clone 0 -resize 20 -colors 256 -compress none \)	\
    \( -clone 0 -resize 24 -colors 256 -compress none \)	\
    \( -clone 0 -resize 32 -colors 256 -compress none \)	\
    \( -clone 0 -resize 40 -colors 256 -compress none \)	\
    \( -clone 1 -resize 48 -colors 256 -compress none \)	\
    \( -clone 1 -resize 96 -colors 256 -compress none \)	\
    \( -clone 1 -resize 128 -colors 256 -compress none \)	\
    \( -clone 0 -resize 16 -compress none \)	\
    \( -clone 0 -resize 20 -compress none \)	\
    \( -clone 0 -resize 24 -compress none \)	\
    \( -clone 0 -resize 32 -compress none \)	\
    \( -clone 0 -resize 40 -compress none \)	\
    \( -clone 1 -resize 48 -compress none \)	\
    \( -clone 1 -resize 64 -compress none \)	\
    \( -clone 1 -resize 96 -compress none \)	\
    \( -clone 1 -resize 128 -compress none \)	\
    \( -clone 1 -resize 256 -compress Zip \)	\
    -delete 1 -delete 0	\
    -alpha remove ../icons/windows-icons/jtdx.ico
rm /tmp/image-0.png /tmp/image-1.png
identify -format '%f %p/%n %m %C/%Q %r %G %A %z\n' ../icons/windows-icons/jtdx.ico
#
inkscape -z -e installer_logo.png -w 150 -h 57 -b white installer_logo.svg
convert png:installer_logo.png -resize 150x57 +matte BMP3:../icons/windows-icons/installer_logo.bmp
rm installer_logo.png
identify -format '%f %p/%n %m %C/%Q %r %G %A %z\n' ../icons/windows-icons/installer_logo.bmp

#
# Mac
#
mkdir -p ../icons/Darwin/wsjtx.iconset
inkscape -z -e ../icons/Darwin/wsjtx.iconset/icon_16x16.png -w 16 -h 16 jtdx_globe_128x128.svg
inkscape -z -e ../icons/Darwin/wsjtx.iconset/icon_16x16@2x.png -w 32 -h 32 jtdx_globe_128x128.svg
inkscape -z -e ../icons/Darwin/wsjtx.iconset/icon_32x32.png -w 32 -h 32 jtdx_globe_128x128.svg
inkscape -z -e ../icons/Darwin/wsjtx.iconset/icon_32x32@2x.png -w 64 -h 64 jtdx_globe_128x128.svg
inkscape -z -e ../icons/Darwin/wsjtx.iconset/icon_128x128.png -w 128 -h 128 jtdx_globe_1024x1024.svg
inkscape -z -e ../icons/Darwin/wsjtx.iconset/icon_128x128@2x.png -w 256 -h 256 jtdx_globe_1024x1024.svg
inkscape -z -e ../icons/Darwin/wsjtx.iconset/icon_256x256.png -w 256 -h 256 jtdx_globe_1024x1024.svg
inkscape -z -e ../icons/Darwin/wsjtx.iconset/icon_256x256@2x.png -w 512 -h 512 jtdx_globe_1024x1024.svg
inkscape -z -e ../icons/Darwin/wsjtx.iconset/icon_512x512.png -w 512 -h 512 jtdx_globe_1024x1024.svg
inkscape -z -e ../icons/Darwin/wsjtx.iconset/icon_512x512@2x.png -w 1024 -h 1024 jtdx_globe_1024x1024.svg
identify -format '%f %p/%n %m %C/%Q %r %G %A %z\n' ../icons/Darwin/wsjtx.iconset/*
# generic globe iconset for utilities
#mkdir -p ../icons/Darwin/wsjtx.iconset
#inkscape -z -e ../icons/Darwin/wsjtx.iconset/icon_16x16.png -w 16 -h 16 wsjt_globe_128x128.svg
#inkscape -z -e ../icons/Darwin/wsjtx.iconset/icon_16x16@2x.png -w 32 -h 32 wsjt_globe_128x128.svg
#inkscape -z -e ../icons/Darwin/wsjtx.iconset/icon_32x32.png -w 32 -h 32 wsjt_globe_128x128.svg
#inkscape -z -e ../icons/Darwin/wsjtx.iconset/icon_32x32@2x.png -w 64 -h 64 wsjt_globe_128x128.svg
#inkscape -z -e ../icons/Darwin/wsjtx.iconset/icon_128x128.png -w 128 -h 128 wsjtx_globe_1024x1024.svg
#inkscape -z -e ../icons/Darwin/wsjtx.iconset/icon_128x128@2x.png -w 256 -h 256 wsjtx_globe_1024x1024.svg
#inkscape -z -e ../icons/Darwin/wsjtx.iconset/icon_256x256.png -w 256 -h 256 wsjtx_globe_1024x1024.svg
#inkscape -z -e ../icons/Darwin/wsjtx.iconset/icon_256x256@2x.png -w 512 -h 512 wsjtx_globe_1024x1024.svg
#inkscape -z -e ../icons/Darwin/wsjtx.iconset/icon_512x512.png -w 512 -h 512 wsjtx_globe_1024x1024.svg
#inkscape -z -e ../icons/Darwin/wsjtx.iconset/icon_512x512@2x.png -w 1024 -h 1024 wsjtx_globe_1024x1024.svg
#identify -format '%f %p/%n %m %C/%Q %r %G %A %z\n' ../icons/Darwin/wsjtx.iconset/*
# generic globe iconset for utilities
mkdir -p ../icons/Darwin/wsjt.iconset
inkscape -z -e ../icons/Darwin/wsjt.iconset/icon_16x16.png -w 16 -h 16 wsjt_globe_128x128.svg
inkscape -z -e ../icons/Darwin/wsjt.iconset/icon_16x16@2x.png -w 32 -h 32 wsjt_globe_128x128.svg
inkscape -z -e ../icons/Darwin/wsjt.iconset/icon_32x32.png -w 32 -h 32 wsjt_globe_128x128.svg
inkscape -z -e ../icons/Darwin/wsjt.iconset/icon_32x32@2x.png -w 64 -h 64 wsjt_globe_128x128.svg
inkscape -z -e ../icons/Darwin/wsjt.iconset/icon_128x128.png -w 128 -h 128 wsjt_globe_1024x1024.svg
inkscape -z -e ../icons/Darwin/wsjt.iconset/icon_128x128@2x.png -w 256 -h 256 wsjt_globe_1024x1024.svg
inkscape -z -e ../icons/Darwin/wsjt.iconset/icon_256x256.png -w 256 -h 256 wsjt_globe_1024x1024.svg
inkscape -z -e ../icons/Darwin/wsjt.iconset/icon_256x256@2x.png -w 512 -h 512 wsjt_globe_1024x1024.svg
inkscape -z -e ../icons/Darwin/wsjt.iconset/icon_512x512.png -w 512 -h 512 wsjt_globe_1024x1024.svg
inkscape -z -e ../icons/Darwin/wsjt.iconset/icon_512x512@2x.png -w 1024 -h 1024 wsjt_globe_1024x1024.svg
identify -format '%f %p/%n %m %C/%Q %r %G %A %z\n' ../icons/Darwin/wsjt.iconset/*
#
inkscape -z -e "../icons/Darwin/DragNDrop Background.png" -w 640 -h 480 -b white "DragNDrop Background.svg"
identify -format '%f %p/%n %m %C/%Q %r %G %A %z\n' "../icons/Darwin/DragNDrop Background.png"

#
# KDE & Gnome
#
#inkscape -z -e ../icons/Unix/wsjtx_icon.png -w 128 -h 128 wsjtx_globe_1024x1024.svg
#identify -format '%f %p/%n %m %C/%Q %r %G %A %z\n' ../icons/Unix/wsjtx_icon.png
inkscape -z -e ../icons/Unix/wsjtx_icon.png -w 128 -h 128 jtdx_globe_1024x1024.svg
identify -format '%f %p/%n %m %C/%Q %r %G %A %z\n' ../icons/Unix/wsjtx_icon.png
