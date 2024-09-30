### Facial Finetuning: Using Pretrained Image Classification Models to Predict Politicians' Success
This is the repository for the paper "Facial Finetuning: Using Pretrained Image Classification Models to Predict Politicians' Success" by Asbjørn Lindholm, Christian Hjorth, and Julian Schuessler. 

Please visit the [Political Science Research and Methods (PSRM) Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/CH9AXM) for data and replication code for figures and tables in the paper. 

### Models
The fine-tuned facial trait models are available for download at [NextCloud](https://nx2461.your-storageshare.de/s/4DqMCppze4QSErJ). 

When making use of the models in your research, please cite the associated paper:
> Lindholm, Asbjørn, Christian Hjorth, and Julian Schuessler. 2024. “Facial Finetuning: Using Pretrained Image Classification Models to Predict Politicians’ Success.” Political Science Research and Methods: 1–11. doi: 10.1017/psrm.2024.38.




# Repository content


## Data

- **`OMI_training_data.RData`** This file contains the training, validation, and test data used for fine-tuning the CNN models.
- **`Candidate_data.RData`** Candidate data with model predictions from the Danish Local Election in 2021 and the Danish General Election in 2022. When making use of the data in your research, please cite the associated working paper.

## Images
Images from the [One Million Impressions dataset](https://github.com/jcpeterson/omi) divided into training, validation and test subdirectories. 

## Scripts

- **`Get_OMI_Data.R`** This script downloades OMI data and images, and divides the images into train, validation, and test subdirectories.
- **`Attractivness_finetune_model.R`** Script for fine-tuning ResNet-50 CNN for predicting attractiveness.
- **`Trustworthiness_finetune_model.R`** Script for fine-tuning ResNet-50 CNN for predicting trustworthiness.
- **`Dominance_finetune_model.R`** Script for fine-tuning ResNet-50 CNN for predicting dominance.
- **`Evaluation_metrics.R`** Script for calculating evaluation metrics.
- **`How_to_use_model.R`** Script showing how to use the models to make predictions on images.
