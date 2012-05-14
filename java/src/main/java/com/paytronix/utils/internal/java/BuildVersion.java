//
// Copyright 2004-2012 Paytronix Systems, Inc.
// All Rights Reserved
// One or more patents pending
//

package com.paytronix.utils.internal.java;

import java.net.JarURLConnection;
import java.net.URL;
import java.util.jar.Attributes;
import java.util.jar.JarFile;
import java.util.jar.Manifest;
import java.util.StringTokenizer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Hold the version and build numbers.
 */
public class BuildVersion {

    private static final Logger logger = LoggerFactory.getLogger( BuildVersion.class );

   /**
    * Load the version information from the manifest of the JAR file
    * containing this BuildVersion class. Some paytronix apps may have
    * multiple jar files and thus could in theory return multiple
    * versions for a single app.  Using this constructor will at least
    * standardize on the least common denominator jar, common.jar.
    */
    public BuildVersion() throws Exception {
        initializeFromManifest( BuildVersion.class, "BuildVersion.class" );
    }

   /**
    * Load the version information from the manifest of the JAR file
    * containing the passed class.  All paytronix jars have such a
    * manifest file, but others will not.
    * @param c Class object whose classloader will look up the JAR
    * @param className String name of the class whose JAR should be
    * interrogated for the manifest containing version and build numbers,
    * eg, "PxcMain.class" or "BuildVersion.class".
    */
    public BuildVersion( Class c, String className ) throws Exception {
        initializeFromManifest( c, className );
    }

   /**
    * Load the version information from the manifest of the JAR file
    * containing the passed class.  All paytronix jars have such a
    * manifest file, but others will not.
    * @param c Class object whose classloader will look up the JAR
    * @param className String name of the class whose JAR should be
    * interrogated for the manifest containing version and build numbers,
    * eg, "PxcMain.class" or "BuildVersion.class".
    */
    private void initializeFromManifest( Class c, String className ) throws Exception
    {
        Attributes attributes = null;
        URL classUrl = null;
        try {
            classUrl = c.getResource( className );
            JarFile classJarFile = ( (JarURLConnection) classUrl.openConnection() ).getJarFile() ;
            Manifest manifest = classJarFile.getManifest();
            attributes = manifest.getMainAttributes();
        } catch (Exception e) {
            String msg = "Failed to read manifest file for " + classUrl;
            logger.error( msg );
            throw new RuntimeException( msg, e );
        }


        versionString = attributes.getValue("Implementation-Version");
        if (versionString == null)
        {
            throw new Exception("No product version info available.  Please verify the JAR file.");
        }

        // Parse the version string of format.
        // Implementation-Version: @VERSION@ @BUILD@
        // into @VERSION@ and @BUILD@ vars. Store the vars locally.

        logger.info("Version=" + versionString
                    + "   className=" + className
                    + "   classUrl=" + classUrl);
        StringTokenizer st = new StringTokenizer( versionString );
        version = st.nextToken();
        build   = st.nextToken();

        if ( version == null || build == null ) {
            throw new Exception("Manifest file does not contain '@VERSION@ @BUILD@'.  Found " + versionString);
        }
    }

    /** Get the full version string of JAR file containing the class used
     * to construct this object. Contains version and build numbers.
     */
    public String getVersionString() { return versionString; }

    /** Get the version number of JAR file containing the class used
     * to construct this object.
     * @return version number in format #.#, eg 4.1
     */
    public String getVersion() { return version; }

    /** Get the build number of JAR file containing the class used
     * to construct this object.
     * @return build number in format ####, eg 1204
     */
    public String getBuild() { return build; }

    // version info
    private String versionString;
    private String version;
    private String build;
}

