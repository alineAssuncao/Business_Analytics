# Instala os pacotes requeridos

# Lista de pacotes usados no projeto
packages <- c("dplyr", "randomForest", "ROCR")

for (p in packages) {
	if(!p %in% rownames(installed.packages())) {
		install.packages(p, dependencies = c("Depends", "Suggests"))
	}
}
