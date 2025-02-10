package it.gov.pagopa.onboarding.workflow.repository;

import it.gov.pagopa.onboarding.workflow.model.Onboarding;
import it.gov.pagopa.onboarding.workflow.model.SelfDeclarationText;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.ArrayList;
import java.util.List;

@ExtendWith({SpringExtension.class, MockitoExtension.class})
@ContextConfiguration(classes = SelfDeclarationSpecificRepositoryImpl.class)
class SelfDeclarationTextSpecificRepositoryTest {

  @Autowired
  SelfDeclarationSpecificRepositoryImpl selfDeclarationSpecificRepository;

  @MockBean
  MongoTemplate mongoTemplate;

  private static final String USER_ID = "TEST_USER_ID";

  @Test
  void deletePaged() {
    String initiativeId = "initiativeId";
    int pageSize = 2;

    List<SelfDeclarationText> selfDeclarationTextList = getSelfDeclarationTexts();

    Mockito.
            when(mongoTemplate.findAllAndRemove(Mockito.any(Query.class), Mockito.eq(SelfDeclarationText.class)))
            .thenReturn(selfDeclarationTextList);

    List<SelfDeclarationText> deletedGroups = selfDeclarationSpecificRepository.deletePaged(initiativeId, pageSize);

    Mockito.verify(mongoTemplate, Mockito.times(1)).findAllAndRemove(Mockito.any(Query.class),Mockito.eq(SelfDeclarationText.class));

    Assertions.assertEquals(selfDeclarationTextList, deletedGroups);
  }

  @NotNull
  private static List<SelfDeclarationText> getSelfDeclarationTexts() {
    SelfDeclarationText onboarding1 = new SelfDeclarationText();
    onboarding1.setUserId(USER_ID);
    onboarding1.setInitiativeId("initiativeId1");
    SelfDeclarationText onboarding2 = new SelfDeclarationText();
    onboarding2.setUserId(USER_ID);
    onboarding2.setInitiativeId("initiativeId2");
    SelfDeclarationText onboarding3 = new SelfDeclarationText();
    onboarding3.setUserId(USER_ID);
    onboarding3.setInitiativeId("initiativeId3");

    List<SelfDeclarationText> selfDeclarationTextList = new ArrayList<>();
    selfDeclarationTextList.add(onboarding1);
    selfDeclarationTextList.add(onboarding2);
    selfDeclarationTextList.add(onboarding3);
    return selfDeclarationTextList;
  }
}
