# remove all of a class
remove_all <- Vectorize(function(class_name) {
    all_objs <- parse_query(class_name)
    if (!is.null(all_objs)) {
        parse_delete(all_objs)
    }
})
