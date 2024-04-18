scmp_write_h5 <- function(
    scmpObj, file_path, overwrite = FALSE, verbose = TRUE) {
  # Check for the file
  if (file.exists(file_path) & !overwrite) {
    stop("File already exists. Please set overwrite = TRUE.")
  } else {
    unlink(file_path)
  }

  # Create File
  h5createFile(file_path)

  # Create all required Slots
  h5createGroup(file_path, group = "dense")
  h5createGroup(file_path, group = "sparse")
  h5createGroup(file_path, group = "design")
  h5createGroup(file_path, group = "profile")
  h5createGroup(file_path, group = "estimate")
  h5createGroup(file_path, group = "significant")
  h5createGroup(file_path, group = "parameters")
  h5createGroup(file_path, group = "misc")

  # Add Misc Details
  ## Add Package Version
  h5write(as.character(packageVersion("scMaSigPro")),
    file = file_path, name = "misc/package_version",
    write.attributes = T,
    createnewfile = F, native = TRUE
  )

  ## Add R Version
  h5write(R.version$version.string,
    file = file_path,
    name = "misc/r_version",
    write.attributes = T,
    createnewfile = F, native = TRUE
  )
  
  # Write Date
  h5write(as.character(Sys.Date()),
          file = file_path,
          name = "misc/date",
          write.attributes = T,
          createnewfile = F, native = TRUE
  )
  
  # Write os
  h5write(as.character(Sys.info()[["sysname"]]),
          file = file_path,
          name = "misc/os",
          write.attributes = T,
          createnewfile = F, native = TRUE
  )

  # Convert to named list
  parameter_list <- as.list(showParams(scmpObj)[["value"]])
  names(parameter_list) <- showParams(scmpObj)[["parameters"]]

  # Write Parameters
  parameter_list <- lapply(names(parameter_list), function(key) {
    # Get value
    value <- parameter_list[[key]]

    # Slot name
    slot_name <- paste0("parameters/", key)

    # Write to file
    h5write(value,
      file = file_path,
      name = slot_name,
      write.attributes = T,
      createnewfile = F, native = TRUE
    )

    return(NULL)
  })

  # Write Sparse and Dense Matrix
  for (sce_slot in c("dense", "sparse")) {
    # stop()

    ## Get higer order names
    if (sce_slot == "dense") {
      slot_names <- names(scmpObj@Dense@assays)
    } else if (sce_slot == "sparse") {
      slot_names <- names(scmpObj@Sparse@assays)
    }

    # Write raw counts
    for (i in slot_names) {
      # stop()

      # Create Slot
      h5createGroup(file_path, group = paste0(sce_slot, "/", i))

      if (sce_slot == "dense") {
        csr_mat <- as(scmpObj@Dense@assays@data@listData[[i]], "dgCMatrix")
      } else if (sce_slot == "sparse") {
        csr_mat <- as(scmpObj@Sparse@assays@data@listData[[i]], "dgCMatrix")
      }

      ## data
      h5write(
        obj = as.vector(csr_mat@x),
        file = file_path,
        name = paste0(sce_slot, "/", i, "/data"),
        write.attributes = T,
        createnewfile = F, native = TRUE
      )

      ## indices
      h5write(
        obj = as.integer(csr_mat@i),
        file = file_path,
        name = paste0(sce_slot, "/", i, "/indices"),
        write.attributes = T,
        createnewfile = F, native = TRUE
      )

      ## indptr
      h5write(
        obj = as.integer(csr_mat@p),
        file = file_path,
        name = paste0(sce_slot, "/", i, "/indptr"),
        write.attributes = T,
        createnewfile = F, native = TRUE
      )

      ## dim
      h5write(as.integer(csr_mat@Dim),
        file = file_path,
        name = paste0(sce_slot, "/", i, "/dim"),
        write.attributes = T,
        createnewfile = F, native = TRUE
      )

      ## Create Slot for names
      h5createGroup(file_path, group = paste0(sce_slot, "/", i, "/labels"))

      ## Write Ids
      ## feature ids
      h5write(
        obj = c(csr_mat@Dimnames[[1]]),
        file = file_path,
        name = paste0(sce_slot, "/", i, "/labels/feature_ids"),
        write.attributes = T,
        createnewfile = F, native = TRUE
      )
      ## cell ids
      h5write(
        obj = c(csr_mat@Dimnames[[2]]),
        file = file_path,
        name = paste0(sce_slot, "/", i, "/labels/cell_ids"),
        write.attributes = T,
        createnewfile = F, native = TRUE
      )
    }
  }

  # Write Design Matrix
  ## Create hig level slots
  h5createGroup(file_path, group = "design/predictors")
  h5createGroup(file_path, group = "design/assigments")

  # Write offsets
  h5write(
    obj = as.vector(scmpObj@Design@offset),
    file = file_path,
    name = "design/offset",
    write.attributes = T,
    createnewfile = F, native = TRUE
  )

  # Write group_vectors
  h5write(
    obj = as.character(scmpObj@Design@groups.vector),
    file = file_path,
    name = "design/groups",
    write.attributes = T,
    createnewfile = F, native = TRUE
  )

  # Write predictors
  predictor_columns <- colnames(scmpObj@Design@predictor_matrix)

  # Write
  for (i in predictor_columns) {
    # Write
    h5write(
      obj = as.numeric(scmpObj@Design@predictor_matrix[, i, drop = TRUE]),
      file = file_path,
      name = paste0("design/predictors/", i),
      write.attributes = T,
      createnewfile = F, native = TRUE
    )
  }

  # Write assignments
  assignment_columns <- colnames(scmpObj@Design@assignment_matrix)

  # Write
  for (i in assignment_columns) {
    # Write
    h5write(
      obj = as.integer(scmpObj@Design@assignment_matrix[, i, drop = TRUE]),
      file = file_path,
      name = paste0("design/assigments/", i),
      write.attributes = T,
      createnewfile = F, native = TRUE
    )
  }

  # Write estimates
  ## Create high level slots
  h5write(
    obj = as.character(scmpObj@Estimate@path),
    file = file_path,
    name = "estimate/path",
    write.attributes = T,
    createnewfile = F, native = TRUE
  )

  # Write estimates
  estimate_slots <- slotNames(scmpObj@Estimate)[!slotNames(scmpObj@Estimate) %in% c("path", "influential")]

  # Write
  for (i in estimate_slots) {
    # Create Groups
    h5createGroup(file_path, group = paste0("estimate/", i))

    # Get data
    matrix <- slot(scmpObj@Estimate, i)

    # Write
    for (j in colnames(matrix)) {
      # Write
      h5write(
        obj = as.numeric(matrix[, j, drop = TRUE]),
        file = file_path,
        name = paste0("estimate/", i, "/", j),
        write.attributes = T,
        createnewfile = F, native = TRUE
      )
    }
  }
  
  # Write Profiles
  profile_slots <- slotNames(scmpObj@Profile)
  
  # Write
  for (i in profile_slots) {
      # Get data
      vec <- slot(scmpObj@Profile, i)
     
      # Write 
      h5write(
          obj = vec,
          file = file_path,
          name = paste0("profile/", i),
          write.attributes = T,
          createnewfile = F, native = TRUE
      )
  }
  
  # Write Significant
  


  # close the file
  h5closeAll()
}
