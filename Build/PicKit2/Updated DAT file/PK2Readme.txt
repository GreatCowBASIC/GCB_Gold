There are 2 Device files included in this folder.

These device files are intended to support 8-bit microcontrollers with Great Cow Basic and are not optimized for PIC 16 or 32-bit microcontrollers although many are supported in Ver 1.63.150

These device files  files correct problems with previous versions where some 8-bit chips were not detected by the PK2 and PK3 GUI aplications. They also corrects problems where the "config masks" were incorrect for certain chips and where some chips had the wrong Device ID.

Now correctly supports:

    12(L)F 1571
    12(L)F 1572

    16(L)F 1788
    16(L)F 1789


1) PK2Devicefile.dat Ver 1.63.150 : Called PK2DeviceFile_1_63_150_GCB.dat

2) PK2Devicefile.dat Ver 1.63.155 : Called PK2DeviceFile_1_63_155_GCB_ONLY.dat
   Ver 1.163.155 has been modified to remove all devices and device families not supported by Great Cow Basic.  It only supports PIC 8-bit microcontrollers.



** IMPORTANT! **

The following chips are included but with limited support. These chips can be
detected and programmed,  however there is no support for reading or writing Config Word 3.

CONFIG3 configures the watchdog timer. Therefore the watchdog timer
configuration bits will remain at their default settings regardless
of the settings in source code. The watchdog timer is enabled by default.

It is highly recommended to use MPLABX/IPE and a Pickit 3 to program these chips.

12(L)F1612
16(L)F1613
16(L)F1614
16(L)F1615
16(L)F1618
16(L)F1619

============================================

Testing:

Limited testing was done on actual chips as I do not have every possible
chip on hand to test. Here are that chips that were tested.

12F508
12F510
12F683

12F1612 *
12F1571

16F684
16F690
16F886

16F1614 *
16F1615 *
16F1618 *
16F1619 *

16F1825
16F1829
16F1847
16F1788
16F1789
16F1939

18F14K22
18F25K22
18F25K80
18F45K22

* Limited support- See note above.


WMR