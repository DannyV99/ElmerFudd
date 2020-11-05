pipeline {
   agent any
   stages {
      stage('Build') {
         steps {
            echo 'Running build automation'
            sh './gradlew build --no-daemon'
            archiveArtifacts artifacts: '*'
         }
      }
      stage('DeploytoStage') {
         when {
            branch 'master'
         }
         steps {
            withCredentials([usernamePassword(credentialsId: 'webserver_login', usernameVariable: 'USERNAME', passwordVariable: 'USERPASS')]) {
               sshPublisher(
                  failOnError: true,
                  continueOnError: false,
                  publishers: [
                     sshPublisherDesc(
                        configName: 'staging'
                        sshCredentials: [
                           username: "$USERNAME",
                           encryptedPassphrase: "$USERPASS"
                        ],
                        transfers: [
                           sshTransfer(
                              sourceFiles: '*',
                              remoteDirectory: '/home',
                           )
                        ]
                     )
                  ]
               )
            }
         }
      }
   }
}
