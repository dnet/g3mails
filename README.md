G3mails gets mails from public Google Groups by name and tracks previously retrieved ones to only report the ones that made it to the mailing list since the last request.

Message tracking
================

Tracking is done using simple text files, one per group, and every one of them contains the list of permalinks for already seen messages. The file name is generated using the group name, so the tracker file for a group called *hspbp* will be saved as *getmails-alse-hspbp.txt*.

Dependencies
============

The project's only dependency is atomizer, the version capable of parsing Google Groups' atom feed is available at http://github.com/dnet/atomizer. Atomizer depends on erlsom, which can be downloaded from http://erlsom.sourceforge.net/.

License
=======

The whole project is released under the MIT license.
