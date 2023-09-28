package it.gov.pagopa.onboarding.workflow.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import it.gov.pagopa.onboarding.workflow.constants.OnboardingWorkflowConstants;
import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ExtendWith({SpringExtension.class, MockitoExtension.class})
@ContextConfiguration(classes = OnboardingSpecificRepositoryImpl.class)
class OnboardingSpecificRepositoryTest {

  @Autowired
  OnboardingSpecificRepository onboardingSpecificRepository;

  @MockBean
  MongoTemplate mongoTemplate;

  @MockBean
  Criteria criteria;

  private static final String USER_ID = "TEST_USER_ID";
  private static final String INITIATIVE_ID = "TEST_INITIATIVE_ID";
  private static final String STATUS = "STATUS";
  private static final LocalDateTime START_DATE = LocalDateTime.now();
  private static final LocalDateTime END_DATE = LocalDateTime.now();

  @Test
  void findByFilter(){
    Criteria criteria = new Criteria();
    Pageable paging = PageRequest.of(0, 20, Sort.by("lastUpdate"));

    onboardingSpecificRepository.findByFilter(criteria,paging);
    Mockito.verify(mongoTemplate, Mockito.times(1)).find(Mockito.any(),Mockito.any());
  }

  @Test
  void findByFilter_criteria_null(){
    Criteria criteria = new Criteria();

    onboardingSpecificRepository.findByFilter(criteria,null);
    Mockito.verify(mongoTemplate, Mockito.times(1)).find(Mockito.any(),Mockito.any());
  }

  @Test
  void getCount(){
    Criteria criteria = new Criteria();
    onboardingSpecificRepository.getCount(criteria);
    Mockito.verify(mongoTemplate, Mockito.times(1)).count((Query) Mockito.any(),
        (Class<?>) Mockito.any());
  }

  @Test
  void getCriteria(){
    Criteria criteria = onboardingSpecificRepository.getCriteria(INITIATIVE_ID,USER_ID,STATUS,START_DATE,END_DATE);
    assertEquals(4,criteria.getCriteriaObject().size());
  }

  @Test
  void getCriteriaOnlyStartDate(){
    Criteria criteria = onboardingSpecificRepository.getCriteria(INITIATIVE_ID,USER_ID,STATUS,START_DATE,null);
    assertEquals(4,criteria.getCriteriaObject().size());
  }

  @Test
  void getCriteriaOnlyEndDate(){
    Criteria criteria = onboardingSpecificRepository.getCriteria(INITIATIVE_ID,USER_ID,STATUS,null,END_DATE);
    assertEquals(4,criteria.getCriteriaObject().size());
  }
  @Test
  void getCriteriaOnlyInitiativeId(){
    Criteria criteria = onboardingSpecificRepository.getCriteria(INITIATIVE_ID,null,null,null,null);
    assertEquals(1,criteria.getCriteriaObject().size());
  }
  @Test
  void getCriteriaOnlyStatus(){
    Criteria criteria = onboardingSpecificRepository.getCriteria(INITIATIVE_ID,null, OnboardingWorkflowConstants.INVITED,null,null);
    assertEquals(2,criteria.getCriteriaObject().size());
  }
  @Test
  void deletePaged() {
    String initiativeId = "initiativeId";
    int pageSize = 2;

    Onboarding onboarding1 = new Onboarding("initiativeId1",USER_ID);
    Onboarding onboarding2 = new Onboarding("initiativeId2",USER_ID);
    Onboarding onboarding3 = new Onboarding("initiativeId3",USER_ID);

    List<Onboarding> onboardingList = new ArrayList<>();
    onboardingList.add(onboarding1);
    onboardingList.add(onboarding2);
    onboardingList.add(onboarding3);

    Mockito.when(mongoTemplate.findAllAndRemove(Mockito.any(Query.class), Mockito.eq(Onboarding.class)))
            .thenReturn(onboardingList);

    List<Onboarding> deletedGroups = onboardingSpecificRepository.deletePaged(initiativeId, pageSize);

    Mockito.verify(mongoTemplate, Mockito.times(1)).findAllAndRemove(Mockito.any(Query.class),Mockito.eq(Onboarding.class));

    Assertions.assertEquals(onboardingList, deletedGroups);
  }
}
