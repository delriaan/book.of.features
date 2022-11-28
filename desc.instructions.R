people <- { list(
			Chionesu = person(
				given = "Chionesu"
				, family = "George"
				, email = "sapienti.Dei@outlook.com"
				, role = c("aut", "cre")
				)
		)}
this.desc <- desc::desc()
this.desc$get_authors() -> .person
this.desc$del(keys = "Authors@R")

rlang::inject(this.desc$set("Author", !!people$Chionesu))
rlang::inject(this.desc$set("Maintainer", !!people$Chionesu))
this.desc$set_version("1.0.0")
this.desc$set_dep("R (>= 4.1.0)")
this.desc$set_list("Imports", c("book.of.utilities", "data.table", "magrittr", "stringi", "foreach"))
this.desc$set(
	Package = dir(dirname(getwd()), pattern = "feature")
	, Title = "Book of Features"
	, Description = "Package `book.of.features` provides ad-hoc feature-engineering utilities."
	)

paste(people$Chionesu$given, people$Chionesu$family, sep = " ") |> (usethis::use_proprietary_license)()
this.desc$print();
this.desc$write()

