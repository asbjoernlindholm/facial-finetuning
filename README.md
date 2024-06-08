### Facial Finetuning: Using Pretrained Image Classification Models to Predict Politicians' Success
This is the repository for the paper "Facial Finetuning: Using Pretrained Image Classification Models to Predict Politicians' Success" by Asbjørn Lindholm, Christian Hjorth, and Julian Schuessler.

The fine-tuned facial trait models are available for download at [NextCloud](https://nx2461.your-storageshare.de/s/4DqMCppze4QSErJ). 

When making use of the models in your research, please cite the associated working paper:
Lindholm, A., Hjorth, C., & Schuessler, J. (2023). Facial Finetuning: Using Pretrained Image Classification Models to Predict Politicians' Success. https://doi.org/10.31235/osf.io/w6x42.

Please visit the [Political Science Research and Methods (PSRM) Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910%2FDVN%2FCH9AXM&version=DRAFT) for data and replication code for figures and tables in the paper.

# Repository content


## Data
This directory contains the data necessary for fine-tuning the CNN models.

- OMI_training_data.RData: This file contains the training, validation, and test data for fine-tuning the CNN models.
- Candidate_data.RData: Candidate data with model predictions from the Danish Local Election in 2021 and the Danish General Election in 2022.

## Images
This directory contains images from the [One Million Impressions dataset](https://github.com/jcpeterson/omi) divided into training, validation and test subdirectories. 

## Scripts
This folder contains the code for fine-tuning each of the models using images from the [One Million Impressions dataset](https://github.com/jcpeterson/omi).

- Get_OMI_Data.R: This script downloades OMI data and images, and divides the images into train, validation, and test subdirectories.
- Attractivness_finetune_model.R: Script for fine-tuning ResNet-50 CNN for predicting attractiveness.
- Trustworthiness_finetune_model.R: Script for fine-tuning ResNet-50 CNN for predicting trustworthiness.
- Dominance_finetune_model.R: Script for fine-tuning ResNet-50 CNN for predicting dominance.
- How_to_use_model.R: Script showing how to use the models to make predictions on images.
