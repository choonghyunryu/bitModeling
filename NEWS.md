# bitmodeling 0.3.1.9001

## NEW FEATURES

* tidymodels 프레임워크 Vignettes 추가. 

* Binary Classification Vignettes 추가.
  
## MINOR CHANGES

* 프로젝트 README 파일 수정. 
  - Binary Classification 섹션 추가
  


# bitmodeling 0.3.0.

## NEW FEATURES
  
* recipes별 멀티모델 적합 기능 추가. 
  - multi_recipes()
  
* 모델링 결과 보고서 작성 기능 추가.
  - report_model()
  - report_model_recipes()

* Pre-defined classifiers에 logistic regression 추가. 

  
  
# bitmodeling 0.2.2.

## MAJOR CHANGES
  
* 모델적합에 random forest(ranger), xgboost Binary Classification classifier 추가. 
  - multi_classifiers()

* tree/regression 기반의 모델 데이터 전처리 이원화. 
  - get_training()

* ranger, xgboost 모델에서의 feature importance 계산.
  - info_model()  
  - viz_model()  
  
  

# bitmodeling 0.2.1.

## NEW FEATURES

* 적합한 멀티모델 모델 비교 평가를 위한 정보 시각화. 
  - compare_viz_model()  
  
## MAJOR CHANGES

* 최적 모델 생성을 위한 성능평가 메트릭의 사용자 선택 기능 추가.
  - get_training()
  - multi_classifiers()
  
## MINOR CHANGES

* 성능평가를 위한 시각화에 title, subtitle 추가. 
  - viz_model()
  - compare_viz_model()
  
  
  
# bitmodeling 0.2.0.

## NEW FEATURES

* 멀티 모델 생성 기능 추가 및 병렬처리 지원.
  - multi_classifiers()
  
* 적합한 멀티모델 모델 비교 평가를 위한 정보 출력.
  - compare_info_model()  
  
* F0.5 Score 및 F2 Score 함수. 
  - f_meas_05()
  - f_meas_2()
  
* Geometric Mean 함수. 
  - gmean()
  
## MINOR CHANGES

* 모델링 함수에 성능평가 메트릭 추가. 
  - get_training()
  
  

# bitmodeling 0.1.6.

## NEW FEATURES

* 모델링 함수. 
  - get_training()
  
* 적합한 모델 평가를 위한 정보 출력 및 시각화. 
  - info_model()  
  - viz_model()  
  
## BUG FIXES

* eda_numeric 도움말 오류 수정. 
  - eda_numeric()

* 수치 변수 EDA의 trim_quantile 인수 결측치 지원. 
  - eda_numeric()  
  


# bitmodeling 0.1.5.

## NEW FEATURES

* EDA 자동화보고서 Vignettes 작성. 
  
  
  
# bitmodeling 0.1.4.

## NEW FEATURES

* 수치 데이터의 정규화 함수. 
  - step_my_center()
  
* 범주형 데이터의 더미화 함수. 
  - step_my_dummy()
  
* 모델 메타 정의 함수. 
  - get_classifier()

## MAJOR CHAHNGES

* 연속형 데이터 EDA 시 데이터 Trim 기능 및 Outliers 관련 정보 추가.
  - eda_numeric()

## MINOR CHAHNGES

* 범주형 변수나 연속형 변수가 없을 때, 명시적인 메세지 출력 및 에러 처리. 
  - eda_category()
  - eda_numeric()
  
  
  
# bitmodeling 0.1.3

## NEW FEATURES

* target 변수 지정 함수. 
  - target_to()

* Positive 클래스 지정 함수. 
  - set_positive()

* 데이터셋 분리를 위한 설정 함수. 
  - split_dataset()

* 데이터셋 추출 함수. 
  - extract_dataset()
  


# bitmodeling 0.1.2

## NEW FEATURES

* target 변수 분포 출력 함수. 
  - target_table()



# bitmodeling 0.1.1

## MINOIR CHAHNGES

* EDA 함수 target 변수 자동 형변환.
  - eda_category()
  - eda_numeric()



# bitmodeling 0.1.0

## NEW FEATURES

* EDA 자동화 및 보고서 출력 함수.
  - eda_category()
  - eda_numeric()    
