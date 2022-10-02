this.desc <- desc::desc();
this.desc$print();

people =
		list(
			Chionesu = person(
				given = "Chionesu"
				, family = "George"
				, email = "cgeorge@alliancehealthplan.org"
				, role = c("aut", "cre")
			)
			, DScience = person(
				given = "Alliance Health Data Science Department"
				, email = "DScience@alliancehealthplan.org"
			)
		);

this.desc$set("Author", people$Chionesu)
this.desc$set("Maintainer", people$DScience)
this.desc$set("License", "Unicorn Overlords")
this.desc$set("Description", "The Book of Features provides functions to help with feature-engineering tasks.")
this.desc$set("Title", "Book of Features: Feature-engineering helpers")
this.desc$set_dep("data.table")
this.desc$set_dep("magrittr")
this.desc$set_dep("purrr")
this.desc$set_version("1.3.0")
this.desc$write()

