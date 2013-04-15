package agent.controller.impl;

import agent.controller.Controller ;

public final class MainController implements Controller {

	// les vues
//	/**
//	 * @see FenetrePrincipaleVue
//	 */
//	private FenetrePrincipaleVue fenetrePrincipaleVue = null;
//
//	/**
//	 * @see BureauVue
//	 */
//	private BureauVue bureauVue = null;
//
//	/**
//	 * @see ConsoleInterneVue
//	 */
//	private ConsoleInterneVue consoleInterneVue = null;
//
//	/**
//	 * @see EditeurMiniJajaInterneVue
//	 */
//	private EditeurMiniJajaInterneVue editeurMiniJajaVue = null;
//
//	/**
//	 * @see JajaCodeVue
//	 */
//	private JajaCodeVue jajaCodeVue = null;
//
//	/**
//	 * @see PileJajaCodeVue
//	 */
//	private PileJajaCodeVue pileJajaCodeVue = null;
//
//	/**
//	 * @see PileMiniJajaVue
//	 */
//	private PileMiniJajaVue pileMiniJajaVue = null;
//
//	private TasVue tasVue = null;
	
	// les modèles
//	/**
//	 * @see Fichier
//	 */
//	private Fichier fichierModele = null;
//
//	/**
//	 * @see AnalyseSemantique
//	 */
//	private AnalyseSemantique analyseSemantiqueModele = null;
//
//	/**
//	 * @see Compilation
//	 */
//	private Compilation compilationModele = null;
//
//	/**
//	 * @see InterpretationJajaCode
//	 */
//	private InterpretationJajaCode interpretationJajaCodeModele = null;

	/**
	 * Constructeur par défaut
	 * 
	 * @throws Exception
	 */
	public MainController() throws Exception {
		// initialisation des modèles
//		this.fichierModele = new FichierModele();
//		this.analyseSemantiqueModele = new AnalyseSemantiqueModele();
//		this.compilationModele = new CompilationModele();
//		this.interpretationJajaCodeModele = new InterpretationJajaCodeModele();

		// initialisation des vues
//		this.fenetrePrincipaleVue = new FenetrePrincipaleVue(this,
//				gestionnaireMessage);
//		this.bureauVue = new BureauVue(this, gestionnaireMessage);
//		this.consoleInterneVue = new ConsoleInterneVue(this,
//				gestionnaireMessage);
//		this.editeurMiniJajaVue = new EditeurMiniJajaInterneVue(this,
//				gestionnaireMessage);
//		this.jajaCodeVue = new JajaCodeVue(this, gestionnaireMessage);
//		this.pileJajaCodeVue = new PileJajaCodeVue(this, gestionnaireMessage);
//		this.pileMiniJajaVue = new PileMiniJajaVue(this, gestionnaireMessage);
//		this.tasVue = new TasVue(this, gestionnaireMessage);
//		
//		this.bureauVue.ajouterComposant(this.consoleInterneVue.getConsole());
//		this.bureauVue.ajouterComposant(this.editeurMiniJajaVue.getEditeur());
//		this.bureauVue.ajouterComposant(this.jajaCodeVue
//				.getInstructionsJajaCode());
//		this.bureauVue.ajouterComposant(this.pileJajaCodeVue.getPileJajaCode());
//		this.bureauVue.ajouterComposant(this.pileMiniJajaVue.getPileMiniJaja());
//		this.bureauVue.ajouterComposant(this.tasVue.getTas());
//		
//		this.fenetrePrincipaleVue.ajouterComposant(this.bureauVue.getBureau(),
//				BorderLayout.CENTER);

		this.addModelListener();
	}
	
	@Override
	public void addModelListener() throws Exception {
		// TODO Auto-generated method stub
		
	}
	
	@Override
	public void displayViews() {
		// TODO Auto-generated method stub
		
	}
	
	public void closeViews() {
		
		
	}
}
