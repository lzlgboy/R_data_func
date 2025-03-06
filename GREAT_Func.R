GREAT_func <- function(bed_grange = bed_grange, genome = 'hg38') { 
    print("Perfroming GREAT analysis!!!")
    
    
    result <- list()
    suppressWarnings({
    job.GREAT <- submitGreatJob(bed_grange,species = 'hg38')
    result$table.GO.Molecular.Function = getEnrichmentTables(job.GREAT,ontology = "GO Molecular Function")
    result$table.GO.Biological.Process = getEnrichmentTables(job.GREAT,ontology = "GO Biological Process")
    result$table.GO.Cellular.Component = getEnrichmentTables(job.GREAT,ontology = "GO Cellular Component")
    result$table.Mouse.Phenotype = getEnrichmentTables(job.GREAT,ontology = "Mouse Phenotype")
    result$table.HumanP.henotype = getEnrichmentTables(job.GREAT,ontology = "Human Phenotype")
    })
        
    return(result)
}
