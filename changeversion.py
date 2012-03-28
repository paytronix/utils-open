#
# Copyright 2012 Paytronix Systems, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

from __future__ import with_statement

import os
import os.path
import sys
from xml.dom import minidom

if len(sys.argv) < 2:
    sys.stderr.write("usage: %s new-version-number\n" % (sys.argv[0], ))
    sys.exit(1)

new_version = sys.argv[1]

def releasify(version):
    if '-' in version:
        return version[:version.index('-')]
    return version

def snapshotify(version):
    if '-' in version:
        return version[:version.index('-')] + "-SNAPSHOT"
    return version + "-SNAPSHOT"

if new_version == "release":
    compute_new_version = releasify
elif new_version == "snapshot":
    compute_new_version = snapshotify
else:
    value = new_version
    compute_new_version = lambda x: value

if not os.path.exists("pom.xml"):
    sys.stderr.write("Please run this script from the trunk of the repository.\n")
    sys.exit(1)

real_new_version = None

def process_pom(path, is_archetype):
    sys.stdout.write("Processing %r...\n" % (path,))
    doc = minidom.parse(path)

    def extracttext(elem):
        val = ""
        for child in list(elem.childNodes):
            if child.nodeType == minidom.Node.TEXT_NODE:
                val += child.data
        return val

    def maptext(elem, f):
        val = ""
        for child in list(elem.childNodes):
            if child.nodeType == minidom.Node.TEXT_NODE:
                val += child.data
            elem.removeChild(child)
        node = minidom.Text()
        node.data = f(val)
        elem.appendChild(node)

    def childbytag(elem, tag):
        return [el for el in elem.childNodes if el.nodeType == minidom.Node.ELEMENT_NODE and el.nodeName == tag]

    project = None
    for node in doc.childNodes:
        if node.nodeType == minidom.Node.ELEMENT_NODE and node.nodeName == "project":
            project = node
            break
    if project is None:
        sys.stdout.write("Error, unable to locate project node. Exiting.")
        sys.exit(1)
    tags = childbytag(project, "groupId")
    groupId = extracttext(tags[0]) if len(tags) > 0 else ""
    if groupId != "com.paytronix" and not groupId.startswith("com.paytronix.") and (not is_archetype or groupId != "${groupId}"):
        sys.stdout.write("Not changing version due to non-paytronix groupId %s\n" % (groupId, ))
    else:
        for tag in childbytag(project, "version"):
            def handle_new_version(existing_version):
                global real_new_version
                if existing_version == "${version}":
                    return existing_version
                new_version = compute_new_version(existing_version)
                if real_new_version is None:
                    real_new_version = new_version
                return new_version

            maptext(tag, handle_new_version)

    for parent in childbytag(project, "parent"):
        tags = childbytag(parent, "groupId")
        groupId = extracttext(tags[0]) if len(tags) > 0 else ""
        if groupId != "com.paytronix" and not groupId.startswith("com.paytronix."):
            sys.stdout.write("Not changing parent version due to non-paytronix groupId %s\n" % (groupId, ))
        else:
            for tag in childbytag(parent, "version"):
                maptext(tag, compute_new_version)

    with file(path, "w") as f:
        f.write("<?xml version=\"1.0\" ?>\n")
        for node in doc.childNodes:
            node.writexml(f)
            if node.nodeType == minidom.Node.COMMENT_NODE:
                f.write("\n")

for dirpath, dirnames, filenames in os.walk("."):
    for exclude in ("src", "target"):
        if exclude in dirnames:
            dirnames.remove(exclude)
    if "pom.xml" in filenames:
        process_pom(os.path.join(dirpath, "pom.xml"), False)

sys.stdout.write("Writing version.txt\n")
with file("version.txt", "w") as f:
    f.write("%s\n" % (real_new_version, ))
