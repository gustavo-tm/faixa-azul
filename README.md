Este repositório contém os scripts e a estrutura de dados para um projeto de pesquisa acadêmica focado na análise do impacto da implementação de "Faixas Azuis" no número e severidade de sinistros viários, com ênfase na reprodutibilidade e transparência dos resultados.

### 1. Visão Geral do Projeto

Este projeto investiga o efeito da implementação de Faixas Azuis em vias urbanas no município de São Paulo na ocorrência de sinistros de trânsito, com foco especial em acidentes envolvendo motocicletas. Utiliza-se uma abordagem de Diferenças-em-Diferenças (DiD) para estimar o impacto causal, garantindo que as análises sejam robustas e transparentes. A pipeline de dados e análise é gerenciada pelo pacote `targets` do R, e o ambiente de projeto é controlado pelo `renv`, promovendo a reprodutibilidade completa do estudo.

### 2. Estrutura do Repositório

O projeto é organizado da seguinte forma:

* `_targets.R`: O arquivo principal do projeto, definindo a pipeline de execução usando o pacote `targets`. Este script orquestra todas as etapas de processamento de dados e análise, garantindo que cada passo seja executado na ordem correta e que as dependências sejam satisfeitas.
* `renv.lock`: Arquivo gerado pelo `renv` que especifica as versões exatas de todos os pacotes R utilizados no projeto, garantindo a consistência do ambiente de execução.
* `scripts/`: Contém todos os scripts R que executam as diferentes etapas do projeto:
    * `tidy_sinistros.R`: Responsável pela limpeza e pré-processamento dos dados brutos de sinistros viários.
    * `tidy_trechos.R`: Lida com o download e tratamento dos dados de trechos viários do OpenStreetMap, incluindo informações como tipo de via, faixas e limites de velocidade.
    * `trechos_complemento.R`: Calcula informações complementares para os trechos viários, como a proximidade de radares e amenidades.
    * `tidy_faixa_azul.R`: Processa os dados relacionados à implementação das Faixas Azuis, associando-as aos trechos viários.
    * `match.R`: Contém funções para padronizar e realizar o casamento (matching) de logradouros entre diferentes bases de dados (sinistros e trechos viários), utilizando técnicas de fuzzy matching para garantir a correta associação geográfica.
    * `did.R`: Prepara os dados para a análise de Diferenças-em-Diferenças (DiD), incluindo o emparelhamento por Propensity Score, e implementa o modelo DiD, realizando as estimativas de impacto.
    * `descritivas.R`: Gera gráficos e estatísticas descritivas para explorar os dados e visualizar os resultados.
* `dados_brutos/`: Diretório para os dados brutos de entrada (não incluídos no repositório para privacidade/tamanho).
* `dados_tratados/`: Diretório para os dados intermediários e finais gerados pelos scripts (gerados pelo `targets`).
* `output/`: Diretório para os gráficos e relatórios finais (gerados pelo `targets`).

### 3. Fontes de Dados
Os dados utilizados neste projeto incluem:

* **Dados de Sinistros Viários (Infosiga)**: Informações detalhadas sobre sinistros em São Paulo, incluindo localização, data, hora, tipo de veículo envolvido e gravidade.
* **Dados de Vias (OpenStreetMap)**: Informações geográficas e atributos das vias, como limites de velocidade ou número de faixas.
* **Dados de Faixa Azul (autoral)**: Levantamento sobre a localização e data de implementação das Faixas Azuis.

### 4. Reprodutibilidade e Transparência

A reprodutibilidade é um pilar central deste projeto, garantida pelo uso extensivo das seguintes ferramentas:

* **Gerenciamento de Pacotes (`renv`)**: O `renv` assegura que o ambiente de R para o projeto seja consistente e isolado. Isso significa que as versões exatas de todos os pacotes R utilizados são registradas (`renv.lock`), permitindo que qualquer pessoa configure o mesmo ambiente de desenvolvimento, eliminando problemas de compatibilidade e facilitando a replicação dos resultados.
* **Pipeline Automatizada (`targets`)**: O `targets` define e executa a pipeline de pesquisa de forma robusta e rastreável. Ele gerencia as dependências entre os scripts, armazena em cache os resultados de cada etapa e só reexecuta o que é estritamente necessário, otimizando o fluxo de trabalho e garantindo que os resultados são sempre gerados a partir da versão correta dos dados e códigos.

Para reproduzir este projeto, basta ter o R instalado e seguir os passos de execução.

### 5. Como Executar o Projeto

1.  **Clone o Repositório**
2.  **Restaure o Ambiente do Projeto**:
    Abra o R na raiz do projeto e execute `renv::restore()`. Isso instalará todos os pacotes nas versões exatas usadas no desenvolvimento.
3.  **Baixe os dados brutos**: Os dados brutos não estão incluídos no repositório, porque excedem o limite disponibilizado pelo GitHub. Certifique-se de que os arquivos necessários estejam na pasta `dados_brutos/`. Para dados do OpenStreetMap, as funções de download estão incluídas.
4.  **Execute a Pipeline**: Na mesma sessão R, execute `targets::tar_make()`. Isso executará toda a pipeline, desde a limpeza dos dados até a geração dos resultados e gráficos. Os resultados estarão disponíveis na pasta `output/`.

**Tempo de Execução e Otimização**

Devido à natureza complexa do processamento de dados geográficos e à extensão das análises realizadas (incluindo operações de matching e modelos de Diferenças-em-Diferenças), a execução completa da pipeline deste projeto pode levar um tempo considerável, potencialmente excedendo 5 horas, dependendo das especificações do hardware.

Para otimizar o tempo de execução, o projeto utiliza o pacote targets, que suporta computação paralela. É possível configurar o número de workers (processos paralelos) diretamente no script `_targets.R` (na seção `tar_option_set`). Aumentar o número de workers pode reduzir significativamente o tempo total de processamento, aproveitando múltiplos núcleos da CPU."
