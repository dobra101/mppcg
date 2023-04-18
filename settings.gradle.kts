rootProject.name = "mppcg"
include("generator")
include("outputLanguage:java")
include("outputLanguage:prolog")
include("inputLanguage:B:java")
include("inputLanguage:B:prolog")
findProject(":inputLanguage:B:prolog")?.name = "prolog"
