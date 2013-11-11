# http://romainfrancois.blog.free.fr/index.php?trackback/434218
depends <- function(pkg = "lattice"){
    index <- readLines( sprintf("http://cran.r-project.org/web/packages/%s/index.html",pkg) )
    if( any( grepl("Reverse.*depends", index) ) ){
        x <- index[ grep( "Reverse.*depends", index ) + 1L ]
        gsub( "<.*", "", strsplit( x, "<a href.*?>" )[[1L]] )[-1L]
    } else character(0L)
}

seen <- character(0)
graph <- character(0)

rec.depends <- function(pkg){
    dep <- depends(pkg)
    if( !length(dep) ) return(NULL)
    graph <<- c( graph, sprintf( "%s->%s", pkg, dep ) )
    for(p in dep[!dep %in% seen]) rec.depends( p )
    seen <<- c( dep[!dep %in% seen] , seen )
}

rec.depends("lattice")

output <- file( "dep.dot", open = "w" )
writeLines( "digraph G {", output )
writeLines( "   rankdir=LR;", output )
writeLines( sprintf( "%s ; ", graph), output )
