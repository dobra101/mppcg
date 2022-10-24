group = "dobra101.mppcg"
version = "1.0-SNAPSHOT"

plugins {
    kotlin("jvm") version "1.7.10"
    application
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-cli:0.3.5")
    implementation(project("generator"))

    implementation("de.hhu.stups:bparser:2.12.0")
}

application {
    mainClass.set("LauncherKt")
}

tasks {
    named("run") {
        doFirst {
            mkdir("generator/build/generated")
        }
    }
}