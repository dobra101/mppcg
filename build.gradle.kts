import org.apache.tools.ant.taskdefs.condition.Os

plugins {
    kotlin("jvm") version "1.7.10"
    application
}

group = "dobra101.mppcg"
version = "1.0-SNAPSHOT"

// on MacOS, Intellij sometimes has problems receiving the environment variables
val jasper: String = System.getenv("jasper") ?: "/usr/local/sicstus4.7.0/lib/sicstus-4.7.0/bin/jasper.jar"

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-cli:0.3.5")

    implementation(project(":generator"))
    implementation(project(":outputLanguage:java"))
    implementation(project(":outputLanguage:prolog"))

    implementation("de.hhu.stups:bparser:2.12.1")
    testImplementation("org.antlr:stringtemplate:4.0.2")
    testImplementation("io.kotest:kotest-runner-junit5:5.5.5")
    testImplementation("io.kotest:kotest-property:5.5.5")
    testImplementation(files(jasper))
    implementation(kotlin("stdlib-jdk8"))
}

application {
    mainClass.set("dobra101.mppcg.LauncherKt")
}

// needed if ProB is running a background task and the test/benchmark has been canceled via intellij
val killProB = tasks.register("killProB") {
    if (!Os.isFamily(Os.FAMILY_MAC)) {
        exec {
            commandLine("echo", "Killing processes only supported for macOS")
        }
    } else {
        val pidFile = file("$buildDir/pid.txt")
        if (pidFile.exists()) {
            val pid = pidFile.readText()
            pidFile.delete()
            logger.lifecycle("Shutting down old ProB process with pid $pid")
            exec {
                commandLine("kill", pid)
            }
        }
    }
}

val copyJavaLibs = copy {
    from("inputLanguage/B/java/build/libs")
    into("generator/build/generated")
    include("btypes.jar")
}

val copyPrologLibs = copy {
    from("inputLanguage/B/prolog/src/main/resources")
    into("generator/build/generated")
    include("btypes.pl", "ordsets.pl")
}

tasks {
    named("run") {
        dependsOn(killProB)
        dependsOn(":inputLanguage:B:java:build")
        doFirst {
            mkdir("generator/build/generated")
        }
        doLast {
            killProB
            copyJavaLibs
            copyPrologLibs
        }
    }

    named("clean") {
        dependsOn(killProB)
    }

    named("test") {
        dependsOn(":inputLanguage:B:java:build")
        doFirst {
            copyJavaLibs
            copyPrologLibs
        }
    }
}

tasks.withType<Test>().configureEach {
    useJUnitPlatform()
}