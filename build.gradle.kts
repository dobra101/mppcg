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

    testImplementation(kotlin("test"))
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