############################################################################
#
# maven-erlang-plugin - Maven Plug-In to manage Erlang projects.
#
############################################################################

----------------------------------------------------------------------------
Developers
----------------------------------------------------------------------------

* Sven Heyll
* Timo Köpke
* Tobias Schlager
* Olle Törnström

----------------------------------------------------------------------------
Version 2.2.0
----------------------------------------------------------------------------

New features:
o Pre-compile sorting now supports the R15 `callback' attribute. 
o Added support for external config file inclusion in sys.config files. 
o Added automatic compilation of MIB files in either `src/main/mibs/' or
        `mibs/', for OTP and standard layout respectively. Compiled MIBs are
        put in the target `priv' directory.
o Added 'coverageExclude' to coverage reporting, allowing for optional
        exclusion of modules from the coverage report. Useful for generated
        or transformed source code, that may be hard or impossible to test.

Fixed Bugs:
o Translation of .tar.gz dependencies into clean artifact names, solves
        issues with modular projects where sibling projects were not
        properly extracted. Thanks to charpi.
o Made the application resource and upgrade files available during the
        test phase of application projects. This allows unit tests to call
        'application:start/1' or 'application:start/2' on the respective
        application under test.  Issue: 3502210. Thanks to charpi.
o Removed `cover2.erl', now using normal coverage tool, solving issue
        with failing coverage reports when using the Meck mocking library.
        Issue: 3502206. Thanks to charpi.
o Fixed Maven3 compatibility problems regarding lazy project dependency
        resolution. Solves issues with missing erlang POM dependencies in
        release projects.
o Setting proper ownership of Temporary directory, for unpacked dependency
        priv resources, solving problems with un-readable packed priv files.

Changes:
o Enhanced the target system creation and execution by providing new start
        and attach scripts. Updated example documentation for target
        systems.
o Resources may now also use the `${NAME}' property for interpolation of
        the project name into resource files.
o Updated resolution of required Erlang/OTP version, now also allowing an
        "or" concept using pipe "|" separated version, for example
        "R14B*|R15B*" - meaning any R14B or R15B version is valid.
o Profiling is no longer performed from normal EUnit tests, one must write
        test modules with the suffix `_prof.erl' profile code. NOTE: this
        change requires user action for projects that upgrade the plug-in.
        Also see use  of the `-Dtest' parameter as an alternative solution.

----------------------------------------------------------------------------
Version 2.1.0
----------------------------------------------------------------------------

Changes in this version include:

New features:
o The plugin's backend nodes write their output to a logfile name
        'backend.log' located in the build directory. This is useful for
        debugging failed builds and unit test runs.
o Resources will now be uploaded along with the modules and application
        files for the upload and run goals unless no temporary directory
        could be found on the target system. This fact will issue a warning
        message.
o Implemented the goal 'erlang:run' for release projects.
o Unique backend nodes will now be started per project in order to support
        parallel builds of different projects.
o Added validator mojo to check the project credentials for legal values.
o Added 'compileFirst' and 'testCompileFirst' parameters for users to be
        able to define compile priority of project modules manually.
o Added support for the additional test source folder 'test' in erlang-std
        projects.
o Added support for eunit tests embedded into a (main) source module.
        These will now also be executed during the standard test phase.
o Provided an alternative for the ERL_FLAGS environment variable for
        target systems. This variable is useful when different releases
        (nodes) should be run on the same host.
o Modules defining custom behaviours will now be compiled. This avoids
        warnings emitted when behaviours can't be found.
o Added 'erlang:profile' and 'erlang:profiling-report' - eprof based
        profiling and reporting, using the tests in an application project.
o Added 'erlang:relup' to generate sensible release upgrade file
        templates based on installed/deployed versions of a project.
o Added 'erlang:appup' to generate sensible application upgrade file
        templates based on installed/deployed versions of a project.

Fixed Bugs:
o Changed method that adds uploaded resources to the path (patha), solves
        path issues when running a project on a remote node.
o Copying of files will now preserve the executable bit. Uploaded
        resources will be flagged executable by default (on the remote
        machine).
o Avoid to unpack test scope dependencies when building a release project.
o Dialyzer mojo now checks for existing .erl-files before execution.
        Solves the problem with packaging projects not containing any source
        files (only includes).
o Fixed problems when erlang runtime paths contain duplicate slashes
        (sometimes discovered on Linux and MacOS).
o Updated project configuration (POM) to work with Maven 3.
o Enhanced compilation to check for exported "parse_transform/2" and sort
        those modules to the top of the compilation list. Resolves
        compilation dependencies issues.
o Backend nodes will now be started in fully qualified hostname mode
        (-name) using one of the IPv4 addresses configured on the host
        machine. This fixes problems with backend node connectivity in
        certain environments, e.g. problems experienced on MacOS X or in
        networks without running DNS services. In case reading the IP
        addresses from the network interfaces fails the old behaviour
        (-name bla@localhost) is used.
o Fixed false test runner results when executing tests containing test
        generators.
o Fixed packaging problems where the wrong version string was used to
        for comparison when validating and checking dependencies.
        Issue: 3240436.
o Fixed packaging problems of release projects when the backend node has
        several OTP installations in its library directory. Standard OTP
        application dependencies will now be searched for in the code path
        of the current backend node.  Issue: 3240233.

Changes:
o For test execution, the parameter 'test' now also supports a comma 
        separated list of test modules to execute (-Dtest=foo,bar,baz) and
        not only a single value.
o Changed resolution of required Erlang/OTP version to allow a trailing
        wildcard "*" - making it possible to require any maintenance release
        of some specified version, for example "R14B*".
o Changed the working directory of the backend nodes to the project's
        build directory.
o Changed binding for site generation, pre-site now invokes 
        'erlang:coverage' and 'erlang:profile' in order to be able to build
        reports.
o Changed coverage report, splitting it into an 'erlang:coverage' goal, 
        that only produces coverage data TXT-file, and an 
        'erlang:coverage-report' goal, that builds the HTML report.
o Projects will now be run with dependencies by default when running an
        application in remote mode.
o Changed mojo logging to be more intuitive when displaying warnings and
        errors.

----------------------------------------------------------------------------
Version 2.0.0
----------------------------------------------------------------------------

Changes in this version include:

New features:
o Added tool 'eapp2mvn' to deploy non-maven packaged erlang applications
        to a maven repository using configurable coordinates.
        Issue: 3167277.
o Added generated help mojo 'erlang:help' to display information about the
        available goals.
o Added the 'erlang:show-build-info' goal that outputs code paths and 
        include paths used to compile the erlang sources, so that other
        tools (i.e. emacs) can compile erlang files of a mavenized erlang
        project.
o Added UnArchiver support to erlang-rel artifacts. This allows other
        projects to consume these artifacts, e.g. rpm-maven-plugin based
        artifacts.
o Added experimental goal to create a startable target system from a
        release project (according to the official erlang documentation).
o Added goal erlang:upload to upload the applications and releases to a
        remote erlang node (located on another machine).
o Added plain text coverage report as optional output direct to stdout
        instead of saved coverage report.
o Added support for test and provided scope dependencies.
o Fixed run goal erlang:run to start all transitive application
        dependencies and pre-load all project (and dependency) modules.
        The goal can now run projects on remote nodes (located on another
        machine).
o Added more checks for release file management support, removed automatic
        release file generation.
o Added more checks for application file management support, removed
        automatic application file generation.
o Added several unit and integration tests.
o All mojos now use jinterface for erlang rpcs instead of using
        erl -run or erl -eval. First of all this speeds up the build
        significantly. Another big advantage is that the plugin can now be
        integrated into the development using emacs with the distel
        extension since the plugin can use the emacs distel node or vice
        versa.
o Added packaging type erlang-std which respects the default erlang/OTP
        application directory layout.
o Test suffixes can now be either "_test" (for backward compatibility) or
        "_tests" eunit standard.

Fixed Bugs:
o Fixed dangling application project dependencies in 'target/lib' when
        changing a dependency version and not 'cleaning' the next build.
o Added support to recognise both British and American spelling of the
        -behaviour tag.  Issue: 3166359. Thanks to ghaskins.

Changes:
o Made 'relup' and 'sys.config' files mandatory in erlang-rel projects.
        Added basic template generation for those in 'erlang:setup'.
o Changed semantics of the ${ERTS} release packaging variables to expand
        to the complete erts version tuple as required by the release
        file's erts section.
o Changed semantics of the ${APPLICATION_NAME} release packaging
        variables to expand to the complete application name, version
        tuple as required by the release file's applications section.
o Changed semantics of ${APPLICATIONS} for erlang-otp/erlang-std projects.
        This will now expand to a comma separated listing instead of an
        erlang list. Issue: 3166835.
o Release and release upgrade files of erlang-rel projects must now be of
        the form [ARTIFACTID].rel / [ARTIFACTID].relup. This allows usage
        of the maven-release-plugin that changes the effective project
        version in the pom while building. Though, generated release
        artifacts will retain the version information in the filenames.
o Refined dependency management of releases by choosing the standard
        erlang/OTP application versions from a specific OTP release.
        Availability of the required release is checked (but may be
        skipped for testing purposes).  Issue: 3166835.
        Thanks to ghaskins.
o Improved application packaging by adding the customizable ${APPLICATIONS}
        packaging variable which expands to all application dependencies
        (except OTP standard ones).  Issue: 3166182. Thanks to ghaskins.
o Improved release packaging by adding the customizable ${AUTODEPS}
        packaging variable which expands to all release dependencies
        (including OTP standard ones).  Issue: 3165497. Thanks to ghaskins.
o Changed the ${APPLICATIONS} release packaging variable to expand to a
        comma separated listing of all release dependencies (including
        OTP standard ones).  Issue: 3165497. Thanks to ghaskins.
o Introduced two backend nodes. One used for testing and another one for
        compiling, packaging, running projects.

Removed:
o Removed the 'failIfNoTests' parameter for simplifying plug-in usage.
o Removed built-in mock library support. Use external library instead.
        E.g. 'erlymock' by Sven Heyll (https://github.com/sheyll/erlymock).
o Removed support for SNMP resource compilation.
o Main and test resources no longer supported by packaging, replaced with
        main and test priv directories (can be found using
        code:priv_dir/1).

----------------------------------------------------------------------------
Version 1.0.0-beta
----------------------------------------------------------------------------

Changes in this version include:

New features:
o New mojo that copies test-resources, bound to the generate-test-resources
        phase.
o New parameter for the run goal, allowing optional command line parameters
        to be passed to the starting Erlang node.
o Parameter in the package goal, controlling if the temporary directory is
        deleted or not.
o Added mock system (formerly known as ltest_mock, formerly known as
        erlymock). The system is automatically available for test modules
        during the test phase.
o New (fixed) surefire report generation handling each test module as
        separate test suite. Therefore an alternative surefire module is
        provided by the plugin.
o Added run goal for packaging erlang-otp to compile/test and run an
        erlang/OTP application for easy testing.
o Release file generation and management support.
o Application file generation and management support.
o Added initial site documentation, generation of plugin-docs, changes
        and some development/contribution information.
o New coverage report, replacing the old one. Now includes line-coverage
        with annotated source code listings.
o Added support for the maven-release-plugin by using application and
        release packaging variables in .app and .rel files.
o Added possibility to include non-erlang sources into application packages.
o Changed erlang-otp packaging to .tar.gz internally.
o Added several unit and integration tests.
o Rewritten all mojos from the original maven-erlang plugin.
o Initial feature set: Compile project and test sources
o Initial feature set: Execute eunit tests
o Initial feature set: Execute dialyzer on erlang/OTP applications and
        releases
o Initial feature set: Generate site documentation with edoc for project
        and test code as well as surefire test reports and basic test code
        coverage
o Initial feature set: Package erlang/OTP applications
o Initial feature set: Package erlang/OTP releases (consisting of
        erlang/OTP applications)
o Initial feature set: Project setup-goal, with defaults for site and
        changelog, main sources and tests.

Fixed Bugs:
o 'TEST' was not set while compiling sources in testphase for coverage.
        Added setting of 'TEST' macro to the cover2:compile call.
o Fixed various problems in test/compile phase when using include files
        from dependency applications.
o Fixed cover compilation with the export_all option to provide coverage
        reports for non-exported module functions. Therefore an alternative,
        patched cover module is provided by the plugin.
o Fixed different problems with release packaging.

Changes:
o Changed the use of the "test" property, a single specified test 
        (-Dtest=some_test) is always considered to have a .beam suffix, but
        never assumes anything else about the test module name.
o Changed dialyzer to run on sources not beams.
