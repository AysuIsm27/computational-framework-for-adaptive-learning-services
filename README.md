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
## Service Definitions

- **Visualize.hs**  
  Defines the generic visualization service. A learner input and a visualization
  model are mapped to a rendered output. Includes a batch variant for rendering
  across a cohort.

- **Assess.hs**  
  Defines the generic assessment service as two composed steps: selecting an
  assessment item for the learner, and interpreting the learner's response as
  evidence about their knowledge state. Includes variants for running multiple
  assessment rounds and for stopping when a mastery criterion is met.

- **GiveFeedback.hs**  
  Defines the generic feedback service. A student product and an evaluation model
  are mapped to a feedback result. Includes a batch variant for evaluating a set
  of submissions and an iterative variant for refining a product until feedback
  meets a criterion or a round limit is reached.

- **Predict.hs**  
  Defines the generic prediction service. A learner input and a predictive model
  are mapped to a predicted outcome. Includes cohort-level variants for group level
  prediction and for flagging at-risk learners.

- **Sequence.hs**  
  Defines the generic sequencing service. A learner state and a sequencing model
  are mapped to the next recommended activity. Includes variants for generating
  full learning paths and for simulating a sequence of steps.

- **Recommend.hs**  
  Defines the generic recommendation service as two composed steps: determining
  the set of eligible items for a learner, and ranking them from most to least
  suitable. The generic function composes both steps to produce an ordered list
  of recommendations.

---
## Implementations

- **Visualize Brusilovsky et al.hs**  
  Implements a visualization service based on the MasteryGrids Open Social Learner
  Modeling system. Renders a dashboard showing a student's own mastery alongside
  peer models and class averages. Demonstrates a visualization service grounded in
  a learner model and a peer-learner model.

- **Assess Lim et al.hs**  
  Implements a Gamified Heutagogical Multi-Modal AI-driven (GHMA) assessment service.
  Learners self-select challenges from gamified non-linear learning paths and create
  individualized multimodal artefacts; learning analytics interpret responses as
  evidence about mastery. Demonstrates an assessment service grounded in a learner
  model.

- **Give Feedback Long et al.hs**  
  Implements an open learner model feedback service that scaffolds self-regulated
  learning in an intelligent tutoring system for linear equations. Presents
  self-assessment prompts after each problem, then reveals updated skill bars as
  feedback on self-assessment accuracy, and provides a level-progress summary for
  problem selection. Demonstrates a feedback service grounded in a learner model.

- **Predict Akcapinar et al.hs**  
  Implements a student performance prediction service using learning analytics data.
  Uses a learner model built from early course activity to predict final outcomes
  and identify students at risk of failure. Demonstrates a prediction service
  grounded in a learner model.

- **Sequence Ahmadaliev et al.hs**  
  Implements a learning activity sequencing service based on learner knowledge state.
  Uses a weighted overlay learner model to order instructional content according to
  the student's current mastery level and learning mode. Demonstrates a sequencing
  service grounded in a learner model.

- **Recommend Implementations.hs**  
  Implements four recommendation services in a single file, each grounded in a
  different computational model: item response theory (Chen et al.), collaborative
  filtering (Nguyen et al.), latent Dirichlet allocation (Jagan et al.), and
  formative assessment analytics (Rodriguez-Martinez et al.). All four share the
  same generic recommendation interface. Demonstrates recommendation services
  grounded in learner/ peer learner models.

```bash
stack build
```
