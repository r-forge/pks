## Competency deletion procedure


## Helper functions ----------------------------------------------------------


hasLetters <- function(C1, C2) {
  letts <- strsplit(C1, "")[[1]]
  all(sapply(letts, grepl, C2, fixed = TRUE))
}

create.bag.matrix <- function(cc.vec, CS, DesignMat) {
  rownames(DesignMat) <- colnames(DesignMat) <- CS
  idcie <- which(DesignMat == 1, arr.ind = TRUE)
  bg <- matrix(0L, nrow(idcie), length(cc.vec),
    dimnames = list(paste(rownames(DesignMat)[idcie[, 1]],
                          colnames(DesignMat)[idcie[, 2]], sep = "/"),
                    cc.vec)
  )
  for (i in 1:nrow(bg)) {
    for (k in 1:ncol(bg)) {
      if(xor(hasLetters(cc.vec[k], rownames(DesignMat)[idcie[i, 1]]),
             hasLetters(cc.vec[k], colnames(DesignMat)[idcie[i, 2]])))
        bg[i, k] <- 1L
    }
  }
  bg[order(rownames(bg)), ]
}


## Print methods -------------------------------------------------------------


print.DeletionRule <- function(x, ...){
    print(cbind(x), ...)
    invisible(x)
}

print.cdp <- function(x, ...){
    cat("\nCompetency Deletion Procedure (CDP) Results\n")
    cat("\nAchieved Reduct:\n") 
    cat(x$reduct)
    cat("\n")
    cat("\nNumber of Iterations:", x$niter)
    cat("\nDeleted Competencies in chronological order:\n")
    cat(x$ordered_deletions)

    cat("\n\n")
    cat(x$pref_rel_method)
    cat("\nPreference Relation:\n")
    cat(apply(cbind(x$pref_rel), 1, paste, collapse = " "), sep = "\n")

    if(x$verbose) {
        cat("\n")
        cat("\nBag Matrix (Delta)\n")
        print(x$delta)
        cat("\n")
        cat("\nUsed Competencies (T):\n")
        cat(x$Competencies)
        cat("\n\nUsed Competence Structure (CS):\n")
        print(x$CS)
        cat("\nUsed Design Matrix:\n")
        print(x$DesignMat)
    }
    cat("\n")
    invisible(x)
}


## CDP function --------------------------------------------------------------


## Competency Deletion Procedure
cdp <- function(Competencies, CS, prefRel = c("NSP", "MBP", "MSP", "FSP"), 
    DesignMat = NULL, verbose = FALSE)
{
    ### Input Checks:
    if (ncol(CS) > 26) {
        stop("'CS' must not contain more than 26 skills")
    }
    if (!is.character(Competencies)) {
        stop("'Competencies' must be
             a character vector containing the competencies")
    }
    if(length(Competencies) != length(unique(Competencies))){
        Competencies <- unique(Competencies)
        warning("'Competencies' should consist of
                unique competency elements. Duplicates were deleted")
    }
    if (!is.matrix(CS)) {
        stop("'CS' must be a matrix containing the competency states")
    }
    if (length(as.pattern(CS)) != length(unique(as.pattern(CS)))){
        CS <- suppressWarnings(CS[as.pattern(CS) == unique(as.pattern(CS)), ])
        warning("'CS' should consist of
                unique competency states as rows. Duplicates were deleted")
    }
    if (inherits(prefRel, "DeletionRule")) {
        v <- trimws(unlist(strsplit(paste(prefRel, collapse = ","), ",")))
        v <- v[order(nchar(v), v)]
        if (length(v) < length(Competencies) || 
            !all(Competencies[order(nchar(Competencies), 
                                    Competencies)] %in% v)){
            stop("The Preference Relation must include
                 all competencies in 'Competencies'")
        }
        if (length(v) != length(unique(v))){
            stop("Preference Relation must not include
                 duplicates of competencies")
        }
        if (length(v) > length(Competencies)){
            warning("'prefRel' includes more competencies
                    than are included in 'Competencies'")
        }
    }

    check_matrix_input <- function(mat) {   # function to check individally...
        if (!(                              # ...designed DesignMat
            is.null(mat) ||                 # NULL is an acceptable input
            (is.matrix(mat) &&              # is it a matrix
            nrow(mat) == nrow(CS) && 
            ncol(mat) == nrow(CS) &&        # check if dimensions are correct
            all(diag(mat) == 0))       # are all elements in the diagonale = 0?
            )){
        stop(paste0("'DesignMat' must either be NULL or a matrix 
                    with the dimensions nrow(CS) x nrow(CS) 
                    and only 0 on the diagonal."))  
                            # Output if Input does not match expectations
        }
        TRUE
    }
    check_matrix_input(DesignMat)

    if (is.null(DesignMat)) {
        CS_DM <- as.pattern(CS, useNames = TRUE)
        DesignMat <- upper.tri(matrix(0, length(CS_DM), length(CS_DM))) + 0
        colnames(DesignMat) <- rownames(DesignMat) <- CS_DM
    }
    verbose <- as.logical(verbose)
    if (is.na(verbose)){
        stop("'verbose' must be TRUE or FALSE")
    }

    ### create Input Arguments for Output
    Competencies_Out <- Competencies
    CS_Out <- CS
    DesignMatrix_Out <- DesignMat

    ### Projecting every pair of states that should be discerned onto the... 
    ### ...upper triangle of the DesignMat
    up <- upper.tri(DesignMat)
    pmax(DesignMat[up], t(DesignMat)[up])
    DesignMat[up] <- pmax(DesignMat[up], t(DesignMat)[up])
    DesignMat[lower.tri(DesignMat)] <- 0

    ### Creating bag matrix
    CS <- as.pattern(unname(CS), useNames = TRUE)  # enforce canonical names
    bag.mat <- create.bag.matrix(Competencies, CS, DesignMat)
    T.cal <- paste(Competencies, collapse = ",")

    ### Creating Preference Relation with respect to chosen Preference... 
    ### ...Relation/ manually defined Preference Relation from input
    if (inherits(prefRel, "DeletionRule")) {
        prefRel <- rev(prefRel)
        class(prefRel) <- "DeletionRule"
    } else {
        prefRel <- match.arg(prefRel, c("NSP", "MBP", "MSP", "FSP"))
    }

    if (identical(prefRel, "MBP")) {
        freq <- sort(colSums(bag.mat))
        comp <- names(freq)
        # Get Preference Relation
        PrefRel <- rep("", length(unique(freq)))
        names(PrefRel) <- unique(freq)
        # Get iterable Preference Relation
        for (idx in 1:length(freq)){
            r_idx <- which(names(PrefRel) == freq[idx])
            if (PrefRel[r_idx] == "") {
              PrefRel[r_idx] <- comp[idx]
            } else {
              PrefRel[r_idx] <- paste(PrefRel[r_idx], comp[idx], sep = ", ")
            }
        }
    }

    if (identical(prefRel, "MSP") || identical(prefRel, "FSP")) {
        comp <- strsplit(T.cal, ",")[[1]]
        skillNum <- nchar(comp)
        PrefRel <- rep("", length(unique(skillNum)))
        names(PrefRel) <- unique(skillNum)

        for (idx in 1:length(skillNum)){
            row_idx <- which(names(PrefRel) == skillNum[idx])
            if  (PrefRel[row_idx] == "") {
                PrefRel[row_idx] <- comp[idx]
            } else {
                PrefRel[row_idx] <- paste(PrefRel[row_idx], comp[idx], 
                                          sep = ", ")
            }
        }
    }

    if (identical(prefRel, "FSP")) {
        PrefRel <- rev(PrefRel)
    }

    if (identical(prefRel, "NSP")) {
        PrefRel <- "NSP does not use a Preference Relation"
    }

    if (inherits(prefRel, "DeletionRule")) {
        PrefRel <- prefRel
    }

    ##### The actual Competency Deletion Procedure (Start)
    ### Definition of later used variables
    keep <- character()
    seen <- character()
    pool <- 9        
    T.new <- unlist(strsplit(T.cal, ","))
    bag.new <- bag.mat
    rows <- seq_len(nrow(bag.mat))

    ### Starting the Competency Deletion Procedure
    while (length(pool) > 0) {
        ## extract row numbers of rows which contain exactly one "1"
        positions <- which(rowSums(bag.new) == 1)
        ## find the column numbers of the "1" in those rows
        for (i in positions) {
            num <- which(bag.new[i, ] == 1)
            keep <- append(keep, colnames(bag.new)[num])  # protect this column
        }
        keep <- unique(keep)
        ## conclude which column could be deleted with... 
        ## ...respect to protected columns
        pool <- setdiff(T.new, keep)
        pool <- setdiff(pool, seen)  

        ## check if there are deletable columns left
        if (length(pool) > 0) {
          if (identical(prefRel, "NSP")) {   ## CDP with random Preference...
                                             ## ...Relation/ Random/ NSP
            column <- sample(pool, 1)   ## if Yes (Random Option): select... 
                                        ## ...one column randomly and delete it
            bag.new[, column] <- rep(0, nrow(bag.new))
                                    ## remember deleted column for efficiency
            seen <- append(seen, column) 
          } else {                           ## CDP with systematic... 
            for (r in 1:length(PrefRel)) {   ## ... Preference Relation
                pool_c <- strsplit(PrefRel[r], ", ")[[1]]   
                ## if Yes (Systematic Options): Step 1: select one column... 
                ## ...considering the PrefRel and delete it
                draw <- intersect(pool_c, pool)
                if (length(draw) > 0){
                    break
                }
            }
            column <- sample(draw, 1)
            ## if Yes: (Systematic Options): Step 2: If there is a tie: ...
            ## ...select one column randomly and delete it
            bag.new[, column] <- rep(0, nrow(bag.new))
                                ## remember deleted column for efficiency
            seen <- append(seen, column) 
          }
        }
                                    ## if No: end loop
    }

    ### create reduct for Output
    alph <- colnames(bag.new)[colSums(bag.new) > 0]
    ##### The actual Competency Deletion Procedure (End)

    ### Create Preference Relation for Output
    if (inherits(prefRel, "DeletionRule") || prefRel != "NSP") {
        PrefRel <- rev(PrefRel)   # Turn PrefRel the right way
        names(PrefRel) <- NULL
        class(PrefRel) <- "DeletionRule"
    }

    if (!inherits(prefRel, "DeletionRule") && prefRel != "NSP") {
        prr <- paste("Chosen method for creating the preference relation:
                     ", prefRel)
    }

    if (inherits(prefRel, "DeletionRule")) {
        prr <- "Chosen method for creating the preference relation: 
        manually defined"
    }

    if (!inherits(prefRel, "DeletionRule") && prefRel == "NSP") {
        prr <- "Chosen method for creating the preference relation: NSP"
    }

    outout <- list(
      reduct = alph,
      niter = length(seen),
      ordered_deletions = seen,
      delta = bag.new[, alph],
      pref_rel = PrefRel,
      pref_rel_method = prr,
      Competencies = Competencies_Out,
      CS = CS_Out,
      DesignMat = DesignMatrix_Out,
      verbose = verbose
    )
    class(outout) <- "cdp"
    outout
}


## Conversion function -------------------------------------------------------


## Convert deletion rules from classic Matlab format into R format
conv.DR <- function(DR){
    if (inherits(DR, "DeletionRule")){
        DR_split <- strsplit(DR, ",\\s*")
        DR_V2 <- unlist(
        lapply(seq_along(DR_split), function(i) {
            setNames(rep(i, length(DR_split[[i]])), DR_split[[i]])
        }))
        Out <- DR_V2
    }
    if (is.vector(DR) && !inherits(DR, "DeletionRule")){
        DR <- sort(DR)
        Out <- c(DR = sapply(split(names(DR), DR), paste, collapse = ", "))
        Out <- unname(Out)
        class(Out) <- "DeletionRule"
    }
    Out
}

