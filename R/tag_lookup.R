tag_lookup <- function(
  type = "ris"
){
  if(!any(c("ris", "ris_write", "medline") == type)){
    stop("tag_lookup only accepts types 'ris', 'ris_write' or 'medline'")
  }
  ris_list <- switch(type,
    "ris" = {
      list(
        "type" = "TY",
        "author" = c("AU", paste0("A", c(1:5))),
        "author_full" = "AF",
        "year" = c("PY", "Y1"),
        "title" = c("TI", "T1"),
        "journal" = c("JO", "T2", "T3", "SO", "JT", "JF", "JA"),
        "volume" = "VL",
        "issue" = "IS",
        "pages" = c("EP", "BP", "SP"),
        "abstract" = c("AB", "N2"),
        "keywords" = c("KW", "DE"),
        "doi" = c("DO", "DI"),
        "call_number" = "CN",
        "issn" = "SN",
        "url" = "UR",
        "accession" = "AN",
        "institution" = "CY",
        "publisher" = "PB",
        "pubplace" = "PP",
        "address" = "AD",
        "editor" = "ED",
        "edition" = "ED",
        "language" = "LA"
      )
    },
    "ris_write" = {
      list(
        "type" = "TY",
        "author" = "AU",
        "year" = "PY",
        "title" = "TI",
        "journal" = "JO",
        "volume" = "VL",
        "number" = "IS",
        "startpage" = "SP",
        "endpage" = "EP",
        "abstract" = "AB",
        "keywords" = "KW",
      	"doi" = "DO",
        "call" = "CN",
      	"issn" = "SN",
        "url" = "UR",
        "accession" = "AN",
        "institution" = "CY",
        "publisher" = "PB",
      	"pubplace" = "PP",
        "address" = "AD",
        "editor" = "ED",
        "edition" = "ET",
        "language" = "LA",
        "end" = "ER"
      )
    },
    "medline" = {
      list(
        "abstract" = "AB",
        "copyright_info" = "CI",
        "affiliation" = "AD",
        "investigator_affiliation" = "IRAD",
        "article_id" = "AID",
        "author" = "AU",
        "author_id" = "AUID",
        "author_full" = "FAU",
        "book_title" = "BTI",
        "collection_title" = "CTI",
        "conflict_of_interest" = "COI",
        "author_corporate" = "CN",
        "date_created" = "CRDT",
        "date_completed" = "DCOM",
        "date_created" = "DA",
        "date_revised" = "LR",
        "date_published_elec" = "DEP",
        "date_published" = "DP",
        "edition" = "EN",
        "editor" = "ED",
        "editor_full" = "FED",
        "date_added" = "EDAT",
        "gene_symbol" = "GS",
        "general_note" = "GN",
        "grant_number" = "GR",
        "investigator" = "IR",
        "investigator_full" = "FIR",
        "isbn" = "ISBN",
        "issn" = "IS",
        "issue" = "IP",
        "journal_abbreviated" = "TA",
        "journal" = "JT",
        "language" = "LA",
        "location_id" = "LID",
        "manuscript_id" = "MID",
        "mesh_date" = "MHDA",
        "mesh_terms" = "MH",
        "nlm_id" = "JID",
        "references_n" = "RF",
        "abstract_other" = "OAB",
        "copyright_info_other" = "OCI",
        "id_other" = "OID",
        "term_other" = "OT",
        "term_owner_other" = "OTO",
        "owner" = "OWN",
        "pages" = "PG",
        "personal_name_as_subject" = "PS",
        "personal_name_as_subject_full" = "FPS",
        "place_published" = "PL",
        "publication_history_status" = "PHST",
        "publication_status" = "PST",
        "publication_type" = "PT",
        "publishing_model" = "PUBM",
        "pubmed_central_identitfier" = "PMC",
        "pubmed_central_release" = "PMCR",
        "pubmed_id" = "PMID",
        "registry_number" = "RN",
        "substance_name" = "NM",
        "secondary_source_id" = "SI",
        "source" = "SO",
        "space_flight_mission" = "SFM",
        "status" = "STAT",
        "subset" = "SB",
        "title" = "TI",
        "title_transliterated" = "TT",
        "volume" = "VI",
        "volume_title" = "VTI"
      )
    }
  )

  # convert this to a data.frame
  length_list <- lapply(ris_list, length)
  ris_dframe <- data.frame(
    ris = do.call(c, ris_list),
    bib = unlist(lapply(names(ris_list), function(a, lookup){
      rep(a, lookup[[a]])
      },
      lookup = length_list
    )),
    stringsAsFactors = FALSE
  )
  rownames(ris_dframe) <- NULL

  if(type == "ris"){
    ris_dframe$order <- unlist(lapply(
      seq_len(length(ris_list)),
      function(a, lookup){
        rep(a, lookup[[a]])
      },
      lookup = length_list
    ))
    # if(!duplicates){
    #   ris_dframe <- do.call(rbind, lapply(
    #     split(ris_dframe, ris_dframe$order),
    #     function(a){a[1, ]}
    #   ))
    # }
  }

  return(ris_dframe)
}