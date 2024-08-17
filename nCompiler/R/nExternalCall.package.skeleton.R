#' Create a skeleton for a package that lets nCompiler call external C++ code
#' 
#' nExternalCall.package.skeleton automates the creation of a new source package
#' with structure that allows C++ code written for the package to be accessible
#' to nExternalCall functions. 
#' 
#' The package needs to be installed, for example using R CMD INSTALL or 
#' devtools::install, before it can be used with nExternalCall.
#' 
#' The skeleton creation code is based on the Rcpp::Rcpp.package.skeleton 
#' function.
#' 
#' @importFrom Rcpp getRcppVersion
#' @importFrom roxygen2 roxygenise
#' 
#' @export 
#' 
nExternalCall.package.skeleton = function(
    name = 'anExternalCallPackage', path = '.', force = FALSE,
    example_code = TRUE,
    author = "Your Name", 
    maintainer = if (missing(author)) "Your Name" else author,
    email = "your@email.com",
    license = "GPL (>= 2)"
) {
  
  env = new.env(parent = emptyenv())
  
  # build Rcpp plugin for package
  assign(x = 'inlineCxxPlugin', value = function(...) { }, envir = env)
  body(env$inlineCxxPlugin) = substitute(
    expr = {
      Rcpp::Rcpp.plugin.maker(
        libs = sprintf(
          "%s/%s/libs/%s%s",
          installed.packages()[package_name, "LibPath"],
          package_name,
          package_name,
          .Platform$dynlib.ext
        ),
        package = package_name
      )()
    },
    env = list(package_name = name)
  )
  
  # build hooks to register Rcpp plugin
  assign(x = '.onLoad', value = function(...) { }, envir = env)
  body(env$.onLoad) = substitute(
    expr = {
      registerPlugin(package_name, package_name_symbol:::inlineCxxPlugin)
    },
    env = list(package_name = name, package_name_symbol = as.name(name))
  )
  
  # initialize package directories and files
  tryCatch(
    expr = package.skeleton(
      name = name, environment = env, path = path, force = force
    ), 
    error = function(e) {
      stop(
        sprintf(
          "error while calling `package.skeleton` : %s", 
          conditionMessage(e)
        )
      )
    })
  
  message("\nAdding nCompiler external code package settings")
  
  root = file.path(path, name)
  
  # Add Rcpp to the DESCRIPTION
  DESCRIPTION <- file.path(root, "DESCRIPTION")
  if (file.exists(DESCRIPTION)) {
    imports <- sprintf("Rcpp (>= %s)", Rcpp::getRcppVersion())
    x <- cbind(read.dcf(DESCRIPTION),
               "Imports" = paste(imports, collapse = ", "),
               "LinkingTo" = "Rcpp")
    x[, "Author"] <- author
    x[, "Maintainer"] <- sprintf("%s <%s>", maintainer, email)
    x[, "License"] <- license
    x[, "Title"] <- "What the Package Does in One 'Title Case' Line"
    x[, "Description"] <- paste(
      "One paragraph description of what the package does as one or more full",
      "sentences."
    )
    message(" >> added Imports: Rcpp")
    message(" >> added LinkingTo: Rcpp")
    write.dcf(x, file = DESCRIPTION)
  }
  
  # reset NAMESPACE
  unlink(x = file.path(root, 'NAMESPACE'))
  message(" >> reset NAMESPACE")
  
  # remove package.skeleton instructions
  unlink(x = file.path(root, 'Read-and-delete-me'))
  message(paste(" >> removed", file.path(root, 'Read-and-delete-me')))
  
  # clear default documentation
  unlink(x = file.path(root, 'man'), recursive = TRUE)
  message(" >> reset help files")
  
  # add inst/ directories
  dir.create(
    path = file.path(root, 'inst', 'include', 'headers'), 
    recursive = TRUE,
    showWarnings = FALSE
  )
  include_guard = paste(name, 'h', sep = '_')
  notes = c(
    '// add #include directives or C++ definitions here for code that should',
    '// be externally accessible via the Rcpp::depends attribute, i.e.,',
    '// #include "headers/SOME_FILE.h"'
  )
  if(isTRUE(example_code)) {
    notes = c(notes, '', '#include "headers/my_sum.h"')
  }
  lines = c(
    paste('#ifndef', include_guard),
    paste('#define', include_guard),
    '',
    notes,
    '',
    '#endif',
    ''
  )
  include = file.path(root, 'inst', 'include', paste(name, 'h', sep = '.'))
  con = file(description = include, open = 'w')
  writeLines(lines, con)
  close(con)
  message(" >> added inst/include/")
  
  # create package source directories
  dir.create(
    path = file.path(root, 'src'), showWarnings = FALSE, recursive = TRUE
  )
  cppflags = 'PKG_CPPFLAGS = -I../inst/include'
  con = file(description = file.path(root, 'src', 'Makevars'), open = 'w')
  writeLines(cppflags, con)
  close(con)
  con = file(description = file.path(root, 'src', 'Makevars.win'), open = 'w')
  writeLines(cppflags, con)
  close(con)
  message(" >> added Makevars")
  
  # make sure src directory is read during install
  lines = c(
    paste('#include "', name, '.h"', sep = ''),
    'void noop() { return; }'
  )
  src = file.path(root, 'src', paste(name, 'cpp', sep = '.'))
  con = file(description = src, open = 'w')
  writeLines(lines, con)
  close(con)
  message(" >> added basic cpp file")
  
  if(isTRUE(example_code)) {
    # write header
    f = file.path(root, 'inst', 'include', 'headers', 'my_sum.h')
    con = file(description = f, open = 'w')
    include_guard = paste(name, 'my_sum', 'h', sep = '_')
    writeLines(
      c(
        paste('#ifndef', include_guard),
        paste('#define', include_guard),
        '',
        '#include <Rcpp.h>',
        '',
        paste('namespace', name, '{'),
        '',
        '    double my_sum(double * x, std::size_t n);',
        '',
        '}',
        '',
        '#endif',
        ''
      ),
      con
    )
    close(con)
    # write implementation
    f = file.path(root, 'src', 'my_sum.cpp')
    con = file(description = f, open = 'w')
    writeLines(
      c(
        paste('#include "', name, '.h"', sep = ''),
        '',
        paste(
          'double ', name, '::my_sum(double * x, std::size_t n) {', sep = ''
        ),
        '    double res = 0;',
        '    if(n == 0) return res;',
        '    double * end = x + n;',
        '    for(double * it = x; it != end; ++it)',
        '        res += *it;',
        '    return res;',
        '}',
        ''
      ),
      con
    )
    close(con)
    message(" >> added example code")
  }
  
  # add roxygen hooks to R files, to make sure NAMESPACE is correct
  hooks = file.path(root, 'R', paste(name, '-internal.R', sep = ''))
  lines = c(
    "#' @importFrom Rcpp registerPlugin",
    readLines(file(hooks))
  )
  con = file(description = hooks, open = 'w')
  writeLines(lines, con)
  close(con)
  message(" >> added roxygen hooks to .onLoad")
  plugin = file.path(root, 'R', 'inlineCxxPlugin.R')
  lines = c(
    "#' @importFrom Rcpp Rcpp.plugin.maker",
    paste("#' @useDynLib", name, ", .registration=TRUE"),
    readLines(file(plugin))
  )
  con = file(description = plugin, open = 'w')
  writeLines(lines, con)
  close(con)
  message(" >> added roxygen hooks to plugin")
  
  # update NAMESPACE, etc.
  roxygen2::roxygenise(package.dir = root, load_code = 'source')
  message(" >> updated NAMESPACE and help files")
  
  invisible(NULL)
}
