# computational-framework-for-adaptive-learning-services


# Adaptive Learning Services Prototype (Haskell)

This repository contains a prototype implementation of adaptive learning services,
written in Haskell.  
Each file corresponds to an implementation inspired by a specific study in the
literature. Together, they illustrate how visualization, assessment, feedback,
prediction, sequencing, and recommendation services can be defined and instantiated
using a shared set of generic patterns.

The code is provided as supplementary material for peer review.

---

## Files

- **Visualize AlonsoFernandez et al.hs**  
  Implements a teacher-facing monitoring service based on a T-Mon Monitoring Model.
  Uses xAPI-SG game traces to cluster students by play style and flag the most
  at-risk learner for the teacher dashboard. Demonstrates a visualization service
  grounded in a learner model.

- **Assess Lim et al.hs**  
  Implements a Gamified Heutagogical Multi-Modal AI-driven (GHMA) assessment service.
  Learners self-select challenges from gamified non-linear learning paths and create
  individualized multimodal artefacts; learning analytics provide personalized feedback
  on mastery. Demonstrates an assessment service grounded in a learner model.

- **Give Feedback Piech et al.hs**  
  Implements a peer assessment feedback service based on a tuned grader calibration
  model. Corrects raw peer grades for grader bias and weights by grader precision to
  produce a calibrated true-score estimate. Demonstrates a feedback service grounded
  in a peer-learner model.

- **Predict Akcapinar et al.hs**  
  Implements a student performance prediction service using learning analytics data.
  Uses a learner model built from early course activity to predict final outcomes and
  identify students at risk of failure. Demonstrates a prediction service grounded
  in a learner model.

- **Sequence Ahmadaliev et al.hs**  
  Implements a learning activity sequencing service based on learner knowledge state.
  Uses a learner model to order instructional content according to the student's
  current mastery level. Demonstrates a sequencing service grounded in a learner model.

- **Recommend Chen et al.hs**  
  Implements a curriculum sequencing service based on a modified Item Response
  Theory (IRT) model. Uses a learner model to balance task difficulty and learner
  ability. Demonstrates a recommendation service grounded in a learner model.

- **Recommend Jagan et al.hs**  
  Implements a learner-behavior-based recommendation using Latent Dirichlet Allocation
  (LDA) to uncover behavioral patterns. Uses a learner model derived from interaction
  logs. Demonstrates a recommendation service grounded in a learner model.

- **Recommend Nguyen et al.hs**  
  Implements a collaborative filtering approach for course recommendation. Uses a
  peer-learner model built on data from multiple students. Demonstrates a
  recommendation service grounded in a peer-learner model.

- **Recommend Rodriguez et al.hs**  
  Implements a recommendation service for personalized homework based on formative
  assessment of fraction tasks. Uses learner performance data to select the most
  appropriate next task. Demonstrates a recommendation service grounded in a
  learner model.

- **README.md**  
  This file. Provides documentation and instructions.

---

## Requirements
- [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
- Tested with **GHC 9.x**

---

## Installation
Clone or download this repository, then build with:

```bash
stack build
```
