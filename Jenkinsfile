properties([
  [
    $class: 'ThrottleJobProperty',
    categories: ['category'],
    limitOneJobWithMatchingParams: false,
    maxConcurrentPerNode: 4,
    maxConcurrentTotal: 0,
    paramsToUseForLimit: '',
    throttleEnabled: true,
    throttleOption: 'category'
  ],
])

pipeline {
    agent none

    stages {
        stage('Prepare') {
            agent {
                label 'linux'
            }
            steps {
                sh 'docker build -t stdcompat .'
            }
        }
        stage('Build') {
            agent {
                label 'linux'
            }
            steps {
                sh 'docker run --rm --volume $PWD:/workspace stdcompat sh -c \'cd /workspace && eval `opam config env` && make -f Makefile.bootstrap && mkdir build && cd build && ../configure && make\''
                stash name: 'build'
                sh 'ls -R'
            }
        }
        stage('Test') {
            agent {
                label 'linux'
            }
            steps {
                script {
                    def switches = sh (
                        script: 'docker run --rm stdcompat opam switch -s',
                        returnStdout: true
                    ).split('\n')
                    def branches = [:]
                    for (i in switches) {
                        def switch_name = i
                        branches[switch_name] = {
                            node('linux') {
                                unstash 'build'
                                sh "docker run --rm --volume $PWD:/workspace stdcompat sh -c 'cd /workspace && opam config exec --switch $switch_name -- sh -c '\\''mkdir build/$switch_name && cd build/$switch_name && ../../configure && make && make tests && ../../configure --disable-magic && make && make tests'\\'"
                            }
                        }
                    }
                    throttle(['category']) {
                        parallel branches
                    }
                }
            }
        }
        stage('Test windows') {
            agent {
                label 'windows'
            }
            steps {
                bat 'echo foo'
            }
        }
        stage('Deploy') {
            agent {
                label 'linux'
            }
            steps {
                unstash 'build'
                sh 'cd build && make dist'
                archiveArtifacts artifacts: 'build/*.tar.gz', fingerprint: true
            }
        }
    }
}
