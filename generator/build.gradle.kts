plugins {
    kotlin("jvm") version "1.7.10"
}

group = "dobra101.mppcg"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    testImplementation(kotlin("test"))
    implementation("org.jetbrains.kotlin:kotlin-reflect:1.7.10")
    implementation("de.hhu.stups:bparser:2.9.31")
    implementation("org.antlr:stringtemplate:4.0.2")
}

tasks.getByName<Test>("test") {
    useJUnitPlatform()
}