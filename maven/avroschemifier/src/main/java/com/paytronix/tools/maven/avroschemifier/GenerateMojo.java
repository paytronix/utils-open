//
// Copyright 2012 Paytronix Systems, Inc.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

package com.paytronix.tools.maven.avroschemifier;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.lang.annotation.Annotation;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.LinkedList;
import org.apache.maven.artifact.DependencyResolutionRequiredException;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.MavenProjectHelper;
import org.slf4j.impl.StaticLoggerBinder;

/**
 * Generates Avro schema from classes with @schema annotations.
 *
 * @execute phase="compile"
 * @goal generate
 * @phase package
 * @requiresDependencyResolution compile
 * @threadSafe
 */
class GenerateMojo extends AbstractMojo {
	/**
	 * Maven project.
	 *
	 * @parameter default-value="${project}"
	 * @required
	 * @readonly
	 */
	private MavenProject project;

    /**
     * @component
     */
    private MavenProjectHelper projectHelper;

	/**
	 * Directory to scan for classes to schemify.
	 *
	 * @parameter default-value="${project.build.directory}/classes"
	 * @required
	 */
	private File classesDirectory;

	/**
	 * Directory to write the schema file in.
	 *
	 * @parameter default-value="${project.build.directory}"
	 * @required
	 */
	private File outputDirectory;

	/**
	 * Classifier to add to the generated schema file. If specified, the schema will be attached to the project. If not,
	 * it will only be written to the target directory.
	 *
	 * @parameter default-value="schema"
	 * @required
	 */
	private String classifier;

	/**
     * Base output filename.
     *
	 * @parameter default-value="${project.build.finalName}"
	 * @required
	 */
	private String finalName;

	public void execute() throws MojoExecutionException {
		try {
            StaticLoggerBinder.getSingleton().setLog(getLog());

			String[] paths = project.getCompileClasspathElements().toArray(new String[0]);
			URL[] urls = new URL[paths.length];
			for (int i = 0; i < paths.length; ++i) {
				urls[i] = new File(paths[i]).toURI().toURL();
			}
			URLClassLoader classLoader = new URLClassLoader(urls);

			getLog().debug("Classpath for schema generation:");
			for (URL url: urls) {
				getLog().debug("  " + url);
			}

			Class<?> avroSchemaGeneratorClass =
				Class.forName("com.paytronix.utils.interchange.AvroSchemaGenerator", true, classLoader);
			Method scanForSchema = avroSchemaGeneratorClass.getMethod("scanForSchema", File.class, Writer.class);
			Object avroSchemaGenerator = avroSchemaGeneratorClass.newInstance();

			String classifierSuffix = classifier != null && classifier.trim().length() > 0 ? ("-" + classifier) : "";
			File outputFile = new File(outputDirectory, finalName + classifierSuffix + ".json");
			getLog().debug("Writing schema to " + outputFile);
			FileWriter fileWriter = new FileWriter(outputFile);
			try {
				scanForSchema.invoke(avroSchemaGenerator, classesDirectory, fileWriter);
			} finally {
				try {
					fileWriter.close();
				} catch (IOException e) {
					getLog().warn("Failed to close schema writer:", e);
				}
			}

			if (classifier != null && classifier.trim().length() > 0) {
				projectHelper.attachArtifact(project, "json", classifier, outputFile);
			}
		} catch (DependencyResolutionRequiredException e) {
			throw new MojoExecutionException("dependency resolution should already have occurred via @requiresDependencyResolution");
		} catch (Exception e) {
			throw new MojoExecutionException("failed to execute generate goal", e);
		}
	}
}