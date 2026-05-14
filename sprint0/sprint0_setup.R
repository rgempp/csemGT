# =============================================================================
# csemGT — Sprint 0: Setup operativo
# Ejecuta los 12 pasos del mini-spec v1.1 §3.2 con checkpoints intermedios.
#
# Entorno objetivo:
#   - R 4.6.0 (Windows)
#   - Working directory: C:/Users/rene.gempp/Dropbox/SOFT/CondSEM
#   - GitHub user: rgempp
#   - Branch default: master (coherente con personnelSelectionUtility)
#   - Sitio público vía pkgdown en gempp.cl/csemGT/
#
# NOTA: este script NO se ejecuta de corrido. Está pensado para correr por
# bloques (Ctrl+Enter línea-a-línea o sección a sección). Tras pasos
# críticos hay un CHECKPOINT que conviene revisar antes de avanzar.
# =============================================================================


# ---- 0. Configuración del entorno -------------------------------------------
# Rutas absolutas. En Windows R acepta forward slashes; no hace falta escapar
# backslashes. Si el path contiene espacios o tildes Dropbox lo gestiona bien.

PACKAGE_DIR  <- "C:/Users/rene.gempp/Dropbox/SOFT/CondSEM/csemGT"
SPRINT0_DIR  <- file.path(PACKAGE_DIR, "sprint0")   # subcarpeta INTERIOR
                                                     # donde guardaste los
                                                     # 5 artefactos + la copia
                                                     # de trabajo del legacy R
LEGACY_R     <- file.path(SPRINT0_DIR, "csem_gt_estimation_v3.R")

# IMPORTANTE: PACKAGE_DIR ya existe y contiene materiales de trabajo (PDFs
# de papers, .ado de Stata, specs en .md, etc). create_package() agregará
# el esqueleto del paquete (DESCRIPTION, NAMESPACE, R/, .Rproj) ENCIMA de
# lo existente sin tocar tus archivos. Más adelante (pasos 9.5 y 9.6) los
# aislamos vía .Rbuildignore y .gitignore para que NI el build NI el repo
# los toquen — coherente con el requisito de "solo paquete + datos en repo".

setwd(PACKAGE_DIR)

# Verificaciones preflight
stopifnot(
  "PACKAGE_DIR no existe"
      = dir.exists(PACKAGE_DIR),
  "Subcarpeta sprint0/ no encontrada dentro de csemGT/"
      = dir.exists(SPRINT0_DIR),
  "Legacy R script no encontrado en csemGT/sprint0/"
      = file.exists(LEGACY_R),
  "Faltan archivos en sprint0/"
      = all(c("DESCRIPTION", "_pkgdown.yml", "README.Rmd", "NEWS.md",
              "csem_gt_estimation_v3.R")
            %in% list.files(SPRINT0_DIR))
)

# Comprobar usethis y gh (paquetes que probablemente ya tienes; reinstalar
# si vienen de R < 4.6 para evitar inconsistencias de bytecode)
needed <- c("usethis", "devtools", "gert", "gh", "desc", "fs")
to_install <- needed[!vapply(needed, requireNamespace, logical(1),
                             quietly = TRUE)]
if (length(to_install)) install.packages(to_install)

# Comprobar credencial GitHub
who <- tryCatch(gh::gh_whoami(), error = function(e) NULL)
if (is.null(who) || is.null(who$login)) {
  stop("gh::gh_whoami() no devuelve usuario. Ejecuta:\n",
       "  usethis::create_github_token()  # navegador, genera PAT\n",
       "  gitcreds::gitcreds_set()        # pega el PAT\n",
       "y vuelve a intentar.")
}
stopifnot("Usuario gh distinto de rgempp" = identical(who$login, "rgempp"))

cat("Preflight OK\n",
    "  R         : ", R.version.string,            "\n",
    "  PACKAGE   : ", PACKAGE_DIR,                 "\n",
    "  LEGACY_R  : ", LEGACY_R,                    "\n",
    "  SPRINT0   : ", SPRINT0_DIR,                 "\n",
    "  gh user   : ", who$login,                   "\n", sep = "")


# ---- 1. Crear paquete -------------------------------------------------------
usethis::create_package(PACKAGE_DIR, open = FALSE, rstudio = TRUE)

# Activar el proyecto para que las llamadas subsiguientes de usethis apunten
# a csemGT (en lugar del proyecto activo previo).
usethis::proj_set(PACKAGE_DIR, force = TRUE)
setwd(usethis::proj_path())

# CHECKPOINT: estructura mínima generada
fs::dir_tree(usethis::proj_path(), recurse = 1)


# ---- 2. Configurar DESCRIPTION ---------------------------------------------
# Sustituye el DESCRIPTION recién generado por el del mini-spec §3.3 con tus
# datos reales (ORCID, email, URL gempp.cl).
file.copy(
  file.path(SPRINT0_DIR, "DESCRIPTION"),
  usethis::proj_path("DESCRIPTION"),
  overwrite = TRUE
)

# CHECKPOINT: que parsea como DESCRIPTION válido
print(desc::desc(usethis::proj_path("DESCRIPTION")))


# ---- 3. Configurar GitHub --------------------------------------------------
# Esto inicializa git localmente y crea el repo remoto en tu cuenta GitHub.
# Branch default: main (decisión: estandarizar nuevos repos a main; tu repo
# personnelSelectionUtility queda con master sin tocarlo).
usethis::use_git()                # inicia git + primer commit en branch main

usethis::use_github(
  private      = FALSE,
  description  = "Conditional Standard Error of Measurement in Generalizability Theory"
)
# usethis configurará automáticamente el remote 'origin' y empujará el
# branch local (main) al remoto. Si te pregunta por overwrite, elige "no"
# y verifica que el remote no exista todavía.


# ---- 4. Configurar licencia ------------------------------------------------
usethis::use_gpl3_license()
# Esto crea LICENSE/LICENSE.md y modifica la línea License: de DESCRIPTION.
# Verificar que la línea quedó "GPL (>= 3)" tras esto. Si no, recopiar
# DESCRIPTION del paso 2.


# ---- 5. Documentación ------------------------------------------------------
usethis::use_roxygen_md()         # ya estaba en DESCRIPTION; añade Config
usethis::use_news_md(open = FALSE)
usethis::use_readme_rmd(open = FALSE)

# Sustituye los archivos generados por las plantillas finales:
file.copy(file.path(SPRINT0_DIR, "NEWS.md"),
          usethis::proj_path("NEWS.md"),    overwrite = TRUE)
file.copy(file.path(SPRINT0_DIR, "README.Rmd"),
          usethis::proj_path("README.Rmd"), overwrite = TRUE)

# Renderiza README.Rmd -> README.md para que GitHub lo muestre
devtools::build_readme()


# ---- 6. Testing ------------------------------------------------------------
usethis::use_testthat(3)
usethis::use_test("csem_gt",        open = FALSE)
usethis::use_test("class",          open = FALSE)
usethis::use_test("csem_gt_parity", open = FALSE)

# CHECKPOINT: tres placeholders creados
list.files(usethis::proj_path("tests/testthat"), pattern = "^test-")


# ---- 7. GitHub Actions -----------------------------------------------------
usethis::use_github_action("check-standard")
usethis::use_github_action("test-coverage")
usethis::use_github_action("pkgdown")

# Los templates de usethis hacen referencia a main y master por defecto en
# los `on.push.branches`, así que no requieren edición manual.

# CHECKPOINT: tres workflows
list.files(usethis::proj_path(".github/workflows"))


# ---- 8. Coverage badge -----------------------------------------------------
usethis::use_coverage(type = "codecov")
# Después de esto:
#  (a) Ir a https://codecov.io, link with GitHub, autorizar rgempp/csemGT
#  (b) Copiar el upload token y agregarlo en
#      GitHub -> Settings -> Secrets -> Actions -> New repository secret
#      Name: CODECOV_TOKEN
# (Si no lo configuras ahora, el workflow simplemente no subirá coverage;
#  no es bloqueante para Sprint 0.)


# ---- 9. Estructura de directorios ------------------------------------------
dir.create(usethis::proj_path("inst", "legacy"),  recursive = TRUE, showWarnings = FALSE)
dir.create(usethis::proj_path("inst", "extdata"), recursive = TRUE, showWarnings = FALSE)
dir.create(usethis::proj_path("data-raw"),                          showWarnings = FALSE)
dir.create(usethis::proj_path("pkgdown"),                           showWarnings = FALSE)
# pkgdown/ queda lista para el hex sticker cuando lo agregues (Fase B o
# v1.0.1). Por ahora vacía.


# ---- 9.5. .Rbuildignore extendido ------------------------------------------
# Tu PACKAGE_DIR tiene materiales de investigación que no deben ir al build
# del paquete: PDFs de papers, .ado de Stata, specs en .md, etc. Los
# excluimos vía .Rbuildignore. Esto NO afecta git: los archivos siguen
# versionados (si los commiteas), simplemente R CMD build los ignora y
# R CMD check --as-cran no emite NOTE "Non-standard files".

usethis::use_build_ignore(c(
  "^dev$",                                # convención: subcarpeta para
                                          # materiales de desarrollo si
                                          # decides organizarlos
  "^sprint0$",                            # artefactos del Sprint 0
  ".*\\.pdf$",                            # PDFs de papers de referencia
  ".*\\.ado$",                            # archivos Stata
  ".*\\.sthlp$",                          # Stata help files
  ".*\\.dta$",                            # datasets Stata
  ".*\\.do$",                             # Stata scripts
  "^csemR_spec.*\\.md$",                  # spec del paquete completo
  "^csemGT_minispec.*\\.md$",             # spec operacional
  "^Conditional_.*\\.md$",                # docs académicos
  "^metodos_clasicos.*\\.md$"             # notas de métodos clásicos
), escape = FALSE)

# CHECKPOINT: inspeccionar el .Rbuildignore resultante
cat("--- .Rbuildignore ---\n")
cat(readLines(usethis::proj_path(".Rbuildignore")), sep = "\n")
cat("\n")


# ---- 9.6. .gitignore extendido ---------------------------------------------
# Para cumplir el requisito "solo paquete y datos en el repo", excluimos de
# git todo lo que no es contenido del paquete: PDFs de referencia, archivos
# Stata, specs internos, materiales de bootstrap (sprint0/), y el output
# local de pkgdown (docs/, que se reconstruye en cada Action).
#
# Nota: .gitignore patterns SIN anclas matchean a cualquier profundidad,
# así que también cubre el caso de que muevas materiales a subcarpetas.

usethis::use_git_ignore(c(
  "sprint0/",
  "dev/",
  "docs/",
  "*.pdf",
  "*.ado",
  "*.sthlp",
  "*.dta",
  "*.do",
  "csemR_spec*.md",
  "csemGT_minispec*.md",
  "Conditional_*.md",
  "metodos_clasicos*.md"
))

# CHECKPOINT: inspeccionar el .gitignore resultante
cat("--- .gitignore ---\n")
cat(readLines(usethis::proj_path(".gitignore")), sep = "\n")
cat("\n")


# ---- 10. Copiar materiales del proyecto ------------------------------------
file.copy(
  LEGACY_R,
  usethis::proj_path("inst", "legacy", "csem_gt_estimation_v3.R"),
  overwrite = TRUE
)

# CHECKPOINT §3.6 (d): legacy R script idéntico al original
stopifnot(
  identical(
    tools::md5sum(LEGACY_R) |> unname(),
    tools::md5sum(usethis::proj_path("inst", "legacy",
                                     "csem_gt_estimation_v3.R")) |> unname()
  )
)
cat("Legacy script copiado y verificado por MD5\n")


# ---- 11. Revisar DESCRIPTION final -----------------------------------------
# Los pasos 4-8 modifican DESCRIPTION (License, Config/testthat/edition,
# posiblemente algún field más). Verifica que coincida con §3.3.

print(desc::desc(usethis::proj_path("DESCRIPTION")))

# Verificación: que todos los Suggests del spec estén presentes
suggests_spec <- c("boot", "mgcv", "ggplot2", "knitr", "rmarkdown",
                   "testthat", "covr", "spelling", "mirt")
suggests_now  <- desc::desc_get_field("Suggests",
                                      file = usethis::proj_path("DESCRIPTION")) |>
  strsplit(",\\s*") |> unlist() |> trimws() |>
  sub(pattern = "\\s*\\(.*\\)$", replacement = "")
missing <- setdiff(suggests_spec, suggests_now)
if (length(missing)) {
  warning("Suggests faltantes en DESCRIPTION: ",
          paste(missing, collapse = ", "),
          ". Recopia DESCRIPTION desde sprint0/ antes de continuar.")
  file.copy(file.path(SPRINT0_DIR, "DESCRIPTION"),
            usethis::proj_path("DESCRIPTION"),
            overwrite = TRUE)
} else {
  cat("DESCRIPTION Suggests OK\n")
}


# ---- 12. pkgdown -----------------------------------------------------------
usethis::use_pkgdown()
# use_pkgdown() crea un _pkgdown.yml minimal. Lo sobrescribimos con la
# versión que replica el estilo de personnelSelectionUtility.
file.copy(file.path(SPRINT0_DIR, "_pkgdown.yml"),
          usethis::proj_path("_pkgdown.yml"),
          overwrite = TRUE)

# Smoke test local (sin viñetas todavía; va a generar un sitio mínimo):
pkgdown::build_site(usethis::proj_path(), preview = FALSE)
# Esto produce una carpeta docs/ LOCAL útil para inspección. No la
# commiteamos (docs/ ya está en .gitignore desde paso 9.6); el deploy se
# hace vía Action en gh-pages branch.


# =============================================================================
# Commit final + push
# =============================================================================
gert::git_add(".")
gert::git_commit("Sprint 0: setup operativo completo")
gert::git_push()


# =============================================================================
# Configuración manual en GitHub web (única vez, post-push)
# =============================================================================
# Tras el primer push exitoso:
#
# 1. Ve a https://github.com/rgempp/csemGT/settings/pages
#    Source: "Deploy from a branch"
#    Branch: gh-pages  /  (root)
#    Save.
#    Espera ~1 min a que la action pkgdown.yaml haya corrido y creado el
#    branch gh-pages.
#
# 2. (Opcional pero recomendado para mantener la URL gempp.cl/csemGT/)
#    En el mismo Pages settings, en "Custom domain" deja vacío. La URL
#    gempp.cl/csemGT/ se resolverá automáticamente a través del CNAME
#    de tu user-site (rgempp.github.io -> gempp.cl). No necesitas archivo
#    CNAME en csemGT a menos que quieras un subdominio dedicado.
#
# 3. Verificar después de unos minutos:
#    - https://github.com/rgempp/csemGT          -> README con badges
#    - https://gempp.cl/csemGT/                  -> sitio pkgdown
#    - https://github.com/rgempp/csemGT/actions  -> tres workflows verdes


# =============================================================================
# Criterio de aceptación §3.6
# =============================================================================

# (a) devtools::check() sobre el paquete vacío: 0E / 0W / 0N
devtools::document()                              # genera NAMESPACE básico
devtools::check()                                 # esperar clean

# (b) GitHub Actions corren al primer commit
# Inspección visual en https://github.com/rgempp/csemGT/actions

# (c) README en GitHub muestra el anuncio de csemR
# Inspección visual en https://github.com/rgempp/csemGT

# (d) inst/legacy/csem_gt_estimation_v3.R idéntico al del proyecto
# Ya verificado por MD5 en el paso 10.

cat("\n",
    "===============================================\n",
    "Sprint 0 completado. Listo para Sprint 1.\n",
    "Próximo sprint: clase S3 `csem` + helpers (~25 h)\n",
    "===============================================\n", sep = "")
