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
    testImplementation("io.kotest:kotest-runner-junit5:5.5.5")
    implementation("io.kotest:kotest-property:5.5.5")
}

application {
    mainClass.set("dobra101.mppcg.LauncherKt")
}

tasks {
    named("run") {
        doFirst {
            mkdir("generator/build/generated")
        }
    }
}

tasks.withType<Test>().configureEach {
    useJUnitPlatform()
}