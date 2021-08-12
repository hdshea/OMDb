# Utility functions for OMDb API
library(magrittr)

get_base_path <- function(){
    "http://www.omdbapi.com/"
}

set_api_key <- function(api_key=NULL) {
    rval <- FALSE

    if(is.null(api_key)) {
        if(file.exists(".cache/code.csv")) {
            d <- tibble::tibble(read.csv(".cache/code.csv"))
            if(("apikey" %in% names(d)) & (nrow(d) == 1)){
                Sys.setenv(OMDB_API_KEY = d$apikey)
                rval <- TRUE
            }
        }
    } else {
        Sys.setenv(OMDB_API_KEY = api_key)
        rval <- TRUE
    }

    rval
}

get_api_key <- function(){
    rval <- Sys.getenv("OMDB_API_KEY")

    if(identical(rval, "")){
        stop("set_api_key() must be called prior to calls to get_api_key().", call. = FALSE)
    }

    rval
}

### All have Ratings list and Response as well

get_game_cols <- function() {
    c(
        "Title", "Year", "Rated", "Released", "Runtime", "Genre", "Director", "Writer",
        "Actors", "Plot", "Language", "Country", "Awards", "Poster",
        "RottenTomatoes", "Metascore", "imdbRating",
        "imdbVotes", "imdbID", "Type", "DVD", "BoxOffice", "Production", "Website"
    )
}

get_movie_cols <- function() {
    c(
        "Title", "Year", "Rated", "Released", "Runtime", "Genre", "Director", "Writer",
        "Actors", "Plot", "Language", "Country", "Awards", "Poster",
        "RottenTomatoes", "Metascore", "imdbRating",
        "imdbVotes", "imdbID", "Type", "DVD", "BoxOffice", "Production", "Website"
    )
}

get_series_cols <- function() {
    c(
        "Title", "Year", "Rated", "Released", "Runtime", "Genre", "Director", "Writer",
        "Actors", "Plot", "Language", "Country", "Awards", "Poster",
        "RottenTomatoes", "Metascore", "imdbRating",
        "imdbVotes", "imdbID", "Type", "totalSeasons"
    )
}

get_episode_cols <- function() {
    c(
        "Title", "Year", "Rated", "Released", "Runtime", "Genre", "Director", "Writer",
        "Actors", "Plot", "Language", "Country", "Awards", "Poster",
        "RottenTomatoes", "Metascore", "imdbRating",
        "imdbVotes", "imdbID", "Type"
    )
}

get_all_cols <- function() {
    unique(dplyr::union_all(
        get_game_cols(),
        get_movie_cols(),
        get_series_cols(),
        get_episode_cols()
    ))
}

get_by_args <- function(base_path, args) {
    rval <- tibble::tibble(.rows = 0)

    res <- httr::GET(base_path, query = args)

    httr::stop_for_status(res)

    test <- res %>%
        httr::content(as = "parsed")

    if(test$Response == "False") {
        message(test$Error)
    } else {
        # Ratings is a list and when there are multiple entries, as_tibble will create multiple rows
        # So we add flattened Ratings back after flattening
        entry <- tibble::as_tibble(test[names(test)[names(test) != "Ratings"]]) %>%
            dplyr::select(-Response)

        rval <- tibble::tibble(.rows = 1)
        for(col in get_all_cols()) {
            if(col %in% names(entry)) {
                rval[col] <- entry[col]
            } else {
                rval[col] <- "N/A"
            }
        }
        if("Ratings" %in% names(test)) {
            ratings <- unlist(test$Ratings)
            for(idx in seq_along(ratings)) {
                if(ratings[idx] == "Rotten Tomatoes") {
                    rval["RottenTomatoes"] <- stringr::str_remove_all(ratings[idx+1], "%")
                }
            }
        }
        rval <- rval %>%
            dplyr::mutate(
                RottenTomatoes = ifelse(
                    RottenTomatoes == "N/A",
                    NA_integer_,
                    as.integer(RottenTomatoes)),
                Metascore = ifelse(
                    Metascore == "N/A",
                    NA_integer_,
                    as.integer(Metascore)),
                imdbRating = ifelse(
                    imdbRating == "N/A",
                    NA_real_,
                    as.numeric(imdbRating)),
                imdbVotes = ifelse(
                    imdbVotes == "N/A",
                    NA_integer_,
                    as.integer(stringr::str_remove_all(imdbVotes,","))),
                Released = as.Date(Released, format="%d %b %Y"),
                DVD = as.Date(DVD, format="%d %b %Y"),
                totalSeasons = ifelse(
                    totalSeasons == "N/A",
                    NA_integer_,
                    as.integer(totalSeasons)),
                across(where(is.character), function(x) { ifelse(x == "N/A", NA_character_, x) })
            )
    }

    rval
}

get_by_id <- function(id, type=NULL, year=NULL, plot="short", api_key=get_api_key()) {
    args <- list(
        i = id,
        type = type,
        y = year,
        plot = plot,
        r = "json",
        apikey = api_key
    )

    get_by_args(get_base_path(), args)
}

get_by_title <- function(title, type=NULL, year=NULL, plot="short", api_key=get_api_key()) {
    args <- list(
        t = title,
        type = type,
        y = year,
        plot = plot,
        r = "json",
        apikey = api_key
    )

    get_by_args(get_base_path(), args)
}

# if(!set_api_key()) {
#     stop("set_api_key() requies either a passed in OMDb API KEY or one stored in .cache/code.csv",
#          call. = FALSE)
# }
#
# x <- get_by_id("tt1605783")
# y <- get_by_id("tt0453422")
# z <- get_by_id("tt1222324")

