plugins {
    kotlin("jvm") version "1.7.10"
}

group = "dobra101.mppcg"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation(project(":generator"))
    testImplementation(kotlin("test"))
}

tasks.getByName<Test>("test") {
    useJUnitPlatform()
}