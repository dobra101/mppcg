plugins {
    kotlin("jvm") version "1.7.10"
}

group = "dobra101.mppcg"

repositories {
    mavenCentral()
}

val fatJar = task("fatJar", type = Jar::class) {
    archiveBaseName.set("btypes")
    duplicatesStrategy = DuplicatesStrategy.EXCLUDE
    from(configurations.runtimeClasspath.get().map { if (it.isDirectory) it else zipTree(it) })
    with(tasks.jar.get() as CopySpec)
}

tasks {
    "build" {
        dependsOn(fatJar)
    }
}

// TODO: copy files