import org.apache.tools.ant.taskdefs.condition.Os

plugins {
    kotlin("jvm") version "1.7.10"
    application
}

group = "dobra101.mppcg"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-cli:0.3.5")

    implementation(project(":generator"))
    implementation(project(":java"))
    implementation(project(":prolog"))

    implementation("de.hhu.stups:bparser:2.12.1")
    testImplementation("org.antlr:stringtemplate:4.0.2")
    testImplementation("io.kotest:kotest-runner-junit5:5.5.5")
    testImplementation("io.kotest:kotest-property:5.5.5")
}

application {
    mainClass.set("dobra101.mppcg.LauncherKt")
}

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

tasks {
    named("run") {
        dependsOn(killProB)
        doFirst {
            mkdir("generator/build/generated")
        }
        doLast{
            killProB
        }
    }

    named("clean") {
        dependsOn(killProB)
    }
}

tasks.withType<Test>().configureEach {
    useJUnitPlatform()
}